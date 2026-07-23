# ============================================================================
# Fidelity Portfolio Import & Analysis
# ============================================================================
# Purpose: Parse a Fidelity "Positions" CSV export, classify accounts by tax
#          treatment, flag/handle missing cost-basis & purchase-date data,
#          and merge with live or delayed quotes for analysis.
#
# Fidelity export instructions:
#   Accounts & Trade > Positions > (download/export icon, usually top right)
#   This gives "Quantity, Last Price, Current Value, Cost Basis Total,
#   Average Cost Basis, Total Gain/Loss" etc. but NOTE: the standard
#   positions export does NOT include purchase date or lot-level detail.
#   For purchase dates / lots, you need: Accounts & Trade > Positions >
#   click a holding > "Lots" or "Cost Basis" view > export, OR
#   Closed Positions / Realized Gain-Loss reports for sold lots.
#   This script handles both: a positions-level file (no dates) and an
#   optional lot-level file (with dates) that you can left-join in.
# ============================================================================

library(tidyverse)
library(tidyfinance)
library(janitor)   # clean_names() for messy Fidelity headers
library(tcltk)
library(plotly)

source("functions.R")



# ----------------------------------------------------------------------------
# 1. CONFIG: Account number -> tax treatment mapping
# ----------------------------------------------------------------------------
# Fidelity's CSV gives you "Account Name" (e.g. "ROTH IRA", "TRADITIONAL IRA")
# which is usually enough to auto-classify. But account names are sometimes
# inconsistent or custom-nicknamed, so we do BOTH:
#   (a) auto-classify from Account Name via keyword matching
#   (b) let you override specific account numbers explicitly below
#
# Tax treatment categories used throughout:
#   "taxable"      - brokerage / individual / joint accounts
#   "trad_deferred" - Traditional IRA, Rollover IRA, SEP, SIMPLE, 401k (pre-tax)
#   "roth"         - Roth IRA, Roth 401k (tax-free growth, already taxed)
#   "hsa"          - Health Savings Account (triple tax-advantaged)
#   "other"        - anything that doesn't match (flagged for manual review)

classify_tax_treatment <- function(account_name) {
  nm <- str_to_upper(account_name)
  case_when(
    str_detect(nm, "ROTH")                                   ~ "roth",
    str_detect(nm, "TRADITIONAL|ROLLOVER|SEP|SIMPLE|PENSION") ~ "trad_deferred",
    str_detect(nm, "\\b401K\\b|\\b403B\\b")                   ~ "trad_deferred",
    str_detect(nm, "\\bHSA\\b")                               ~ "hsa",
    str_detect(nm, "BROKERAGE|INDIVIDUAL|JOINT|TRUST|TOD")    ~ "taxable",
    TRUE                                                      ~ "other"
  )
}

# Manual overrides: fill in if any account numbers need explicit treatment
# e.g. a "Trust" account name that's actually a traditional IRA in trust, etc.
# Format: tibble(account_number = "...", tax_treatment_override = "...")
manual_overrides <- tibble(
  account_number = character(),
  tax_treatment_override = character()
)

# ----------------------------------------------------------------------------
# 2. READ & CLEAN the Fidelity positions export
# ----------------------------------------------------------------------------

read_fidelity_positions <- function(filepath) {

  raw <- read_csv(filepath, show_col_types = FALSE, na = c("", "--", "n/a", "N/A"))

  df <- raw %>%
    clean_names() %>%   # -> account_number, account_name, symbol, description, etc.
    # Drop Fidelity's trailing "Total of all accounts" summary row and any
    # blank/placeholder rows (e.g. "Pending Activity")
    filter(
      !is.na(account_number),
      !is.na(account_name), # only consider rows with an account name
      !str_detect(str_to_upper(coalesce(description, "")), "PENDING ACTIVITY"),
      !str_detect(str_to_upper(coalesce(symbol, "")), "^TOTAL")
    ) %>%
    mutate(
      # Numeric coercion: Fidelity formats some columns with $ , % signs in
      # certain export variants; readr usually handles plain numbers fine,
      # but we defensively strip non-numeric characters just in case.
      across(
        any_of(c("quantity", "last_price", "current_value",
                  "cost_basis_total", "average_cost_basis",
                  "total_gain_loss_dollar")),
        ~ as.numeric(str_remove_all(as.character(.x), "[\\$,%]"))
      ),
      tax_treatment = classify_tax_treatment(account_name)
    ) %>%
    left_join(manual_overrides, by = "account_number") %>%
    mutate(
      tax_treatment = coalesce(tax_treatment_override, tax_treatment)
    ) %>%
    select(-tax_treatment_override)

  # Flag anything that didn't classify cleanly so you can review it
  unclassified <- df %>% filter(tax_treatment == "other")
  if (nrow(unclassified) > 0) {
    warning(
      "⚠ ", nrow(unclassified), " row(s) could not be auto-classified by tax treatment. ",
      "Account name(s): ", paste(unique(unclassified$account_name), collapse = ", "),
      ". Add these to `manual_overrides` above."
    )
  }

  df
}

# ----------------------------------------------------------------------------
# 3. HANDLE MISSING COST BASIS / PURCHASE DATES
# ----------------------------------------------------------------------------
# Fidelity's standard positions export omits cost basis for some positions
# (common for: money market funds at $1 NAV, very old transferred-in shares,
# certain mutual funds, or anything Fidelity doesn't have full lot history
# for - e.g. shares transferred in from another broker years ago).
#
# Strategy:
#   - Flag every row as basis_known = TRUE/FALSE
#   - For basis_known == FALSE: don't silently impute a fake number into
#     cost_basis_total (that would corrupt tax-impact calculations). Instead,
#     carry it as NA and surface it clearly in a "data quality" report.
#   - Optionally, for rows where average_cost_basis IS present but
#     cost_basis_total is NA (or vice versa), back-fill one from the other
#     using quantity, since they're mechanically related.

flag_missing_data <- function(df) {
  df %>%
    mutate(
      # Back-fill cost_basis_total from average_cost_basis * quantity if needed
      cost_basis_total = case_when(
        is.na(cost_basis_total) & !is.na(average_cost_basis) & !is.na(quantity) ~
          average_cost_basis * quantity,
        TRUE ~ cost_basis_total
      ),
      average_cost_basis = case_when(
        is.na(average_cost_basis) & !is.na(cost_basis_total) & !is.na(quantity) & quantity != 0 ~
          cost_basis_total / quantity,
        TRUE ~ average_cost_basis
      ),
      basis_known = !is.na(cost_basis_total),

      # Purchase date: NOT present in standard positions export at all.
      # If you supply a lot-level file (see merge_lot_data() below), this
      # gets filled in from there. Otherwise it stays NA and gets flagged.
      purchase_date = as.Date(NA),
      purchase_date_known = FALSE,

      # For taxable accounts specifically, missing basis/date matters a lot
      # (affects realized gain/loss and ST vs LT capital gains treatment).
      # For tax-advantaged accounts (Roth/Traditional/HSA), basis matters far
      # less for day-to-day analysis since trades inside them aren't taxable
      # events - flagging this distinction avoids over-alarming you about
      # gaps that don't actually matter for IRA/Roth holdings.
      basis_matters_for_taxes = tax_treatment == "taxable"
    )
}

# ----------------------------------------------------------------------------
# 4. OPTIONAL: merge in lot-level detail (has purchase dates) if you have it
# ----------------------------------------------------------------------------
# Fidelity: Accounts & Trade > Positions > click into a specific holding >
# "Lots" tab usually lets you export lot detail with acquisition dates.
# Expected columns after clean_names(): account_number, symbol, quantity,
# acquired_date (or similar), cost_basis, term (short/long)

merge_lot_data <- function(positions_df, lot_filepath) {
  if (!file.exists(lot_filepath)) {
    message("No lot-level file found at ", lot_filepath, " - skipping date merge.")
    return(positions_df)
  }

  lots_raw <- read_csv(lot_filepath, show_col_types = FALSE, na = c("", "--")) %>%
    clean_names()

  # Fidelity lot exports vary in column naming across account types;
  # try common variants for the date column
  date_col <- intersect(
    c("date_acquired", "acquired_date", "purchase_date", "open_date"),
    names(lots_raw)
  )[1]

  if (is.na(date_col)) {
    warning("Could not find a recognizable purchase-date column in lot file. ",
            "Columns found: ", paste(names(lots_raw), collapse = ", "))
    return(positions_df)
  }

  lots_summary <- lots_raw %>%
    rename(purchase_date_raw = all_of(date_col)) %>%
    mutate(purchase_date_parsed = lubridate::mdy(purchase_date_raw)) %>%
    group_by(account_number, symbol) %>%
    summarise(
      earliest_purchase_date = min(purchase_date_parsed, na.rm = TRUE),
      latest_purchase_date   = max(purchase_date_parsed, na.rm = TRUE),
      n_lots = n(),
      .groups = "drop"
    )

  positions_df %>%
    left_join(lots_summary, by = c("account_number", "symbol")) %>%
    mutate(
      purchase_date = earliest_purchase_date,
      purchase_date_known = !is.na(purchase_date),
      # Long-term cap gains = held > 1 year. Flag where determinable.
      holding_period = case_when(
        !purchase_date_known ~ NA_character_,
        as.numeric(Sys.Date() - purchase_date) > 365 ~ "long_term",
        TRUE ~ "short_term"
      )
    )
}

# ----------------------------------------------------------------------------
# 5. MERGE WITH LIVE/DELAYED QUOTES
# ----------------------------------------------------------------------------
# Uses tidyquant (Yahoo Finance, ~15-20 min delayed, free, no API key).
# Cross-checks against Fidelity's "last_price" so you can see if your
# export is stale relative to current market price.

add_live_quotes <- function(df) {
  symbols <- df %>%
    filter(!is.na(symbol), !str_detect(symbol, "\\*\\*")) %>%  # exclude money market (SPAXX**)
    distinct(symbol) %>%
    pull(symbol)

  quotes <- tryCatch({
#    tq_get(symbols, get = "stock.prices", from = Sys.Date() - 5) %>%
    download_data("Stock Prices", symbols = symbols, start_date = Sys.Date(), end_date = Sys.Date()) %>%
      group_by(symbol) %>%
      slice_max(date, n = 1) %>%
      ungroup() %>%
      select(symbol, quote_date = date, live_price = close)
  }, error = function(e) {
    message("Quote fetch failed for some symbols (delisted, fund-only, or money market). ",
            "Proceeding with Fidelity's last_price for those.")
    tibble(symbol = character(), quote_date = as.Date(character()), live_price = numeric())
  })

  df %>%
    left_join(quotes, by = "symbol") %>%
    mutate(
      price_used = coalesce(live_price, last_price),
      price_source = if_else(!is.na(live_price), "live_quote", "fidelity_export"),
      current_value_recalc = quantity * price_used
    )
}

# ----------------------------------------------------------------------------
# 6. DATA QUALITY REPORT
# ----------------------------------------------------------------------------

data_quality_report <- function(df) {
  cat("\n========== DATA QUALITY REPORT ==========\n\n")

  cat("Accounts found and tax treatment classification:\n")
  df %>%
    distinct(account_number, account_name, tax_treatment) %>%
    arrange(tax_treatment) %>%
    print(n = Inf)

  cat("\nPositions missing cost basis (cannot compute gain/loss):\n")
  missing_basis <- df %>% filter(!basis_known)
  if (nrow(missing_basis) == 0) {
    cat("  None - all positions have cost basis.\n")
  } else {
    missing_basis %>%
      select(account_name, symbol, description, quantity, current_value, basis_matters_for_taxes) %>%
      print(n = Inf)
    cat("  -> basis_matters_for_taxes = TRUE means this gap affects a TAXABLE account\n")
    cat("     and matters for capital gains planning. FALSE means it's in a\n")
    cat("     tax-advantaged account where basis is less consequential day-to-day.\n")
  }

  cat("\nPositions missing purchase date (cannot determine ST vs LT gains):\n")
  missing_date <- df %>% filter(!purchase_date_known, tax_treatment == "taxable")
  if (nrow(missing_date) == 0) {
    cat("  None among taxable-account positions.\n")
  } else {
    missing_date %>%
      select(account_name, symbol, description, quantity, current_value) %>%
      print(n = Inf)
    cat("  -> For these, pull lot-level detail from Fidelity (Positions > click\n")
    cat("     holding > Lots/Cost Basis tab) to get acquisition dates, or check\n")
    cat("     'Unknown'-term cost basis flags directly on Fidelity's site -\n")
    cat("     Fidelity itself often shows these as 'Unknown Term' if it lacks\n")
    cat("     the data, which usually means shares transferred in from another\n")
    cat("     broker without full lot history transfer.\n")
  }

  cat("\n==========================================\n\n")

  invisible(df)
}

# ----------------------------------------------------------------------------
# 7. SUMMARY VIEWS
# ----------------------------------------------------------------------------

summarize_by_tax_treatment <- function(df) {
  df %>%
    group_by(tax_treatment) %>%
    summarise(
      n_positions = n(),
      total_value = sum(current_value, na.rm = TRUE),
      total_cost_basis = sum(cost_basis_total, na.rm = TRUE),
      total_unrealized_gain = total_value - total_cost_basis,
      positions_missing_basis = sum(!basis_known),
      .groups = "drop"
    ) %>%
    arrange(desc(total_value))
}

summarize_by_account <- function(df) {
  df %>%
    group_by(account_number, account_name, tax_treatment) %>%
    summarise(
      n_positions = n(),
      total_value = sum(current_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_value))
}

# ============================================================================
# RUN: Read in the fidelity positions file file
# ============================================================================

filepath <- tk_choose.files(
  default = file.path(path.expand("~"), "Downloads", "", "Portfolio*.csv"),
  caption = "Select the Fidelity csv data file",
  multi = FALSE,
  filters = matrix(c("CSV files", ".csv", "", "*"), 2, 2, byrow = TRUE)
)

if (length(filepath) == 0) {
  stop("No file selected.")
}

positions <- read_fidelity_positions(filepath) %>%
  flag_missing_data()

# If you have a lot-level export with purchase dates, point to it here:
# positions <- merge_lot_data(positions, "fidelity_lots_export.csv")

positions <- positions %>% data_quality_report()

cat("\n--- Portfolio by tax treatment ---\n")
print(summarize_by_tax_treatment(positions))

cat("\n--- Portfolio by account ---\n")
print(summarize_by_account(positions))

# Uncomment to pull live quotes (requires internet access in your environment):
positions <- add_live_quotes(positions)

# Save cleaned data for downstream analysis
cleanfilepath <- dirname(filepath)
write_csv(positions, file.path(cleanfilepath, "positions_cleaned.csv"))

# Show allocation of all securities
show_allocation()
