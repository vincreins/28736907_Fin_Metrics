reb_data <- function(returns_data, Rebalance_Days) {
    rebalance_col <- returns_data %>%
        filter(date %in% Rebalance_Days) %>%
        mutate(RebalanceTime = format(date, "%Y%B")) %>%
        group_by(RebalanceTime) %>%
        arrange(desc(Price)) %>%
        mutate(
            weight = Price / sum(Price) # Calculate weight based on Price
        ) %>%
        ungroup() %>%
        arrange(date)

    return(rebalance_col)
}
