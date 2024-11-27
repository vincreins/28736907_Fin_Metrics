stock_concentration <- function(data, top_n = 10) {
  stock_concentration_ALSI <- data %>%
    group_by(date) %>%
    arrange(desc(J203)) %>%
    slice(1:top_n) %>%
    summarize(top_contribution_ALSI = sum(J203, na.rm = TRUE))
  
  stock_concentration_SWIX <- data %>%
    group_by(date) %>%
    arrange(desc(J403)) %>%
    slice(1:top_n) %>%
    summarize(top_contribution_SWIX = sum(J403, na.rm = TRUE))
  
  stock_concentration <- stock_concentration_ALSI %>%
    inner_join(stock_concentration_SWIX, by = c("date"))
  
  ggplot(stock_concentration, aes(x = as.Date(date))) +
    geom_line(aes(y = top_contribution_ALSI, color = "ALSI")) +
    geom_line(aes(y = top_contribution_SWIX, color = "SWIX")) +
    labs(
      title = paste("Top", top_n, "Stock Concentration (ALSI vs SWIX)"),
      y = paste("Top", top_n, "Contribution"),
      x = "Date",
      color = "Index"
    )
}
