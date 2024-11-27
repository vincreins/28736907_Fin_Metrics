calculate_returns <- function(index_data, exchange_rate_data, usd_zar_returns) {
    index_data %>%
        mutate(date = format(as.Date(date), "%Y-%m")) %>%
        full_join(exchange_rate_data, by = "date") %>%
        na.omit() %>%
        mutate(
            J433_USD = J433 / value,
            ALBI_USD = ALBI / value,
            Global_Return = 0.18 * MSCI_ACWI + 0.12 * Bbg_Agg,
            Local_Return = 0.42 * J433_USD + 0.28 * ALBI_USD,
            Portfolio_Return = Global_Return + Local_Return
        ) %>%
        left_join(usd_zar_returns, by = "date")
}
