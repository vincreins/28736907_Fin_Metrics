

rolling_volatility_hedged <- function(Indexes, ZAR, window_width = 12) {
    library(dplyr)
    library(lubridate)
    library(readr)
    library(zoo)
    library(ggplot2)
    # Ensure the date columns are in Date format
    Indexes <- Indexes %>% mutate(date = as.Date(date))
    ZAR <- ZAR %>% mutate(date = as.Date(date))

    # Calculate local returns for ZAR-denominated indices (Capped SWIX and ALBI)
    Indexes_Local_Returns <- Indexes %>%
        arrange(date) %>%
        mutate(
            J433_Return = J433 / lag(J433) - 1,
            ALBI_Return = ALBI / lag(ALBI) - 1
        )

    # Calculate returns for USD-denominated indices (MSCI ACWI and Global Bond Aggregate)
    Indexes_Local_Returns <- Indexes_Local_Returns %>%
        mutate(
            MSCI_ACWI_Return = MSCI_ACWI / lag(MSCI_ACWI) - 1,
            Bbg_Agg_Return = Bbg_Agg / lag(Bbg_Agg) - 1
        )

    # Remove the first row with NA returns due to lag
    Indexes_Local_Returns <- Indexes_Local_Returns %>% na.omit()

    # Calculate the portfolio returns for the hedged portfolio (currency effects are neutralized)
    Indexes_Local_Returns <- Indexes_Local_Returns %>%
        mutate(
            Global_Return = 0.18 * MSCI_ACWI_Return + 0.12 * Bbg_Agg_Return,
            Local_Return = 0.42 * J433_Return + 0.28 * ALBI_Return,
            Portfolio_Return_Hedged = Global_Return + Local_Return
        )

    # Calculate rolling volatility for the hedged portfolio
    Indexes_Local_Returns <- Indexes_Local_Returns %>%
        arrange(date) %>%
        mutate(
            Rolling_Vol_Hedged = rollapply(
                Portfolio_Return_Hedged,
                width = window_width,
                FUN = var,
                fill = NA,
                align = "right"
            ) * sqrt(12)
        )

    # Plotting the rolling volatility over time
    p <- ggplot(Indexes_Local_Returns, aes(x = date, y = Rolling_Vol_Hedged)) +
        geom_line(color = "blue") +
        labs(
            title = paste0(window_width, "-Month Rolling Volatility of Hedged Portfolio"),
            x = "Date",
            y = "Annualized Rolling Volatility"
        ) +
        theme_minimal()

    # Display the plot
    print(p)
}
