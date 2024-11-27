
roll_returns <- function(data, date_col, return_col, rolling_window = 36) {
    library(dplyr)
    library(ggplot2)
    library(RcppRoll)
    data <- data %>%
        arrange({{date_col}}) %>%
        mutate(Rolling_Returns = RcppRoll::roll_prod(1 + {{return_col}}, rolling_window,
                                                     fill = NA, align = "right")^(12 / rolling_window) - 1) %>%
        group_by({{date_col}}) %>%
        filter(any(!is.na(Rolling_Returns))) %>%
        ungroup()

}
