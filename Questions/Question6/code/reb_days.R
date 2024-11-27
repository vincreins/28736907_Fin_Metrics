reb_days <- function(returns_data) {
    library(xts)
    Rebalance_Days <- returns_data %>%
        mutate(
            Year = format(date, "%Y"),
            Month = format(date, "%b"),
            Day = format(date, "%a")
        ) %>%
        filter(Month %in% c("Jan", "Apr", "Jul", "Oct")) %>%
        select(date, Year, Month, Day) %>%
        unique() %>%
        group_by(Month) %>%
        filter(Day == "Wed") %>%
        group_by(Year, Month) %>%
        filter(date == dplyr::first(date)) %>%
        pull(date)

    return(Rebalance_Days)
}
