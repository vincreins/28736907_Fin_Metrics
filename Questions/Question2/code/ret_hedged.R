hedged_returns <- function(index_data) {
    index_data %>%
        mutate(date = format(as.Date(date), "%Y-%m")) %>%
        na.omit() %>%
        mutate(
            # Use local returns directly without converting using exchange rates
            Global_Return = 0.18 * MSCI_ACWI + 0.12 * Bbg_Agg,
            Local_Return = 0.42 * J433 + 0.28 * ALBI,
            Portfolio_Return_Hedged = Global_Return + Local_Return
        )
}
