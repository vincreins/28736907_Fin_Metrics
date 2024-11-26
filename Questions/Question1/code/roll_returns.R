library(dplyr)
library(ggplot2)
library(RcppRoll)

# Define function to calculate and plot rolling returns
roll_returns <- function(data, date_col, return_col, rolling_window = 36) {
    # Ensure tidy evaluation
    data <- data %>%
        arrange({{date_col}}) %>%
        mutate(Rolling_Returns = RcppRoll::roll_prod(1 + {{return_col}}, rolling_window,
                                                     fill = NA, align = "right")^(12 / rolling_window) - 1) %>%
        group_by({{date_col}}) %>%
        filter(any(!is.na(Rolling_Returns))) %>% # Filter out rows with no rolling returns
        ungroup()

}
