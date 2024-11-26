library(dplyr)
library(ggplot2)
library(lubridate)
library(fmxdat)
library(tbl2xts)
library(PerformanceAnalytics)

# Define the function
cum_returns <- function(data, date_col, value_col) {
    # Ensure the column names are interpreted correctly
    data <- data %>%
        arrange({{date_col}}) %>% # Sort by the date column
        mutate(Returns = {{value_col}} / lag({{value_col}}) - 1, # Calculate periodic returns
               Returns = coalesce(Returns, 0),                  # Replace NA with 0
               Cumulative_Returns = cumprod(1 + Returns))       # Calculate cumulative returns

}


