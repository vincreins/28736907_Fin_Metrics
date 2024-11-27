mean_A <- function(data) {
    data %>%
        group_by(date) %>%
        summarise(mean_returns = mean(Returns, na.rm = TRUE), .groups = "drop") %>%
        tbl_xts()
}
