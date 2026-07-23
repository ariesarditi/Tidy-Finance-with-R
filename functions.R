grand_total <-   function(df,var_name) {
 sum(df[[var_name]],na.rm=TRUE)
}

show_allocation <- function() {
  gt <- grand_total(positions,"current_value")
  p <- positions |> group_by(symbol) |> summarize(shares = sum(quantity),symtot = sum(current_value)) %>% mutate(pct = symtot/gt) %T>% View() %>% 
    ggplot(aes(x = symbol, y = pct)) +
    geom_point() +
    scale_y_continuous(sec.axis = sec_axis(transform = ~.* gt)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ggplotly(p)
}



