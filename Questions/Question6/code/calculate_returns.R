calculate_returns <- function(merged_data) {
    returns_data <- merged_data %>%
        group_by(Name) %>%
        arrange(date) %>%
        mutate(
            Return = log(Price / lag(Price))
        ) %>%
        filter(!is.na(Return)) %>%
        ungroup()

    return(returns_data)
}
