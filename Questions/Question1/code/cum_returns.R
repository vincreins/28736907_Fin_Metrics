
cum_returns <- function(data, date_col, value_col) {
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(fmxdat)
    library(tbl2xts)
    library(PerformanceAnalytics)
    data <- data %>%
        arrange({{date_col}}) %>%
        mutate(Returns = {{value_col}} / lag({{value_col}}) - 1,
               Returns = coalesce(Returns, 0),
               Cumulative_Returns = cumprod(1 + Returns))

}


