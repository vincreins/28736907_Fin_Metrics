reb_days <- function(returns_data) {
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
        filter(date == first(date)) %>%
        pull(date)

    return(Rebalance_Days)
}
