plot_rolling_correlations <- function(data, rolling_window = 252) {
    library(dplyr)
    library(zoo)
    library(ggplot2)


    data <- data %>%
        mutate(
            Correlation_Carry = rollapply(
                data = select(., Carry_Index, ZAR_Rate),
                width = rolling_window,
                FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"),
                by.column = FALSE,
                fill = NA,
                align = "right"
            ),
            Correlation_Dollar = rollapply(
                data = select(., Dollar_Index, ZAR_Rate),
                width = rolling_window,
                FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"),
                by.column = FALSE,
                fill = NA,
                align = "right"
            ),
            Correlation_Volatility = rollapply(
                data = select(., Volatility, ZAR_Rate),
                width = rolling_window,
                FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"),
                by.column = FALSE,
                fill = NA,
                align = "right"
            )
        )


    plot <- ggplot(data, aes(x = date)) +
        geom_line(aes(y = Correlation_Carry, color = "Carry Index")) +
        geom_line(aes(y = Correlation_Dollar, color = "Dollar Index")) +
        geom_line(aes(y = Correlation_Volatility, color = "Volatility")) +
        labs(title = "Rolling Correlations with ZAR Rate",
             x = "Date", y = "Correlation",
             color = "Predictors") +
        theme_minimal()

    return(list(data = data, plot = plot))
}
