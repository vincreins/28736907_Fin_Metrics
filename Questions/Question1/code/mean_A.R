mean_A <- function(data) {
    library(tbl2xts)
    data %>%
        group_by(date) %>%
        summarise(mean_returns = mean(Returns, na.rm = TRUE), .groups = "drop") %>%
        tbl_xts()
}
