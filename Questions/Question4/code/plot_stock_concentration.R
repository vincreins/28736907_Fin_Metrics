plot_stock_concentration <- function(merged_data, ALSI) {
    library(ggplot2)
    library(dplyr)
    library(lubridate)

    stock_concentration <- merged_data %>%
        group_by(date) %>%
        arrange(desc(Weight)) %>%
        slice(1:10) %>%
        summarize(top_contribution = sum(Weight, na.rm = TRUE)) %>%
        ungroup()

    stock_concentration_SWIX <- ALSI %>%
        filter(date >= ymd(20191031), date <= ymd(20241031)) %>%
        group_by(date) %>%
        arrange(desc(J403)) %>%
        slice(1:10) %>%
        summarize(top_contribution_SWIX = sum(J403, na.rm = TRUE)) %>%
        ungroup()

    ggplot() +
        geom_line(data = stock_concentration, aes(x = as.Date(date), y = top_contribution, color = "Snake Oil")) +
        geom_line(data = stock_concentration_SWIX, aes(x = as.Date(date), y = top_contribution_SWIX, color = "SWIX")) +
        labs(
            title = "Top 10 Stock Concentration (SWIX vs Snake Oil)",
            y = "Top 10 Contribution",
            x = "Date",
            color = "Index"
        )
}
