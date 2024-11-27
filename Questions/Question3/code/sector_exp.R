sector_exp <- function(data) {
  sector_exposure <- data %>%
    group_by(Sector, date) %>%
    summarize(
      total_weight_ALSI = sum(J203, na.rm = TRUE),
      total_weight_SWIX = sum(J403, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = starts_with("total_weight"),
      names_to = "Methodology",
      values_to = "total_weight"
    ) %>%
    group_by(Sector, Methodology) %>%
    summarize(mean = mean(total_weight))
  
  ggplot(sector_exposure, aes(x = Sector, y = mean, fill = Methodology)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Sector Exposure",
      y = "Average Weight",
      x = "Sector",
      fill = "Methodology"
    )
}
