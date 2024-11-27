plot_normalized <- function(data) {
    library(ggplot2)

    ggplot(data, aes(x = date)) +
        geom_line(aes(y = ZAR_Rate_norm, color = "Normalized ZAR Rate")) +
        geom_line(aes(y = Carry_Index_norm, color = "Normalized Carry Index")) +
        geom_line(aes(y = Dollar_Index_norm, color = "Normalized Dollar Index")) +
        geom_line(aes(y = Volatility_norm, color = "Normalized Volatility")) +
        labs(
            title = "Normalized Variables Over Time",
            x = "Date",
            y = "Normalized Value",
            color = "Legend"
        ) +
        theme_minimal()
}
