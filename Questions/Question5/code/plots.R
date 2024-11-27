plots <- function(data) {
    library(ggplot2)
    library(gridExtra)


    plot1 <- ggplot(data, aes(x = Carry_Index, y = ZAR_Rate)) +
        geom_point() +
        geom_smooth(method = "lm", color = "blue", se = FALSE) +
        labs(title = "ZAR Rate vs Carry Index",
             x = "Carry Index (DBHVG10U)", y = "ZAR Exchange Rate") +
        theme_minimal()


    plot2 <- ggplot(data, aes(x = Dollar_Index, y = ZAR_Rate)) +
        geom_point() +
        geom_smooth(method = "lm", color = "green", se = FALSE) +
        labs(title = "ZAR Rate vs Dollar Index",
             x = "Dollar Index (BBDXY)", y = "ZAR Exchange Rate") +
        theme_minimal()


    plot3 <- ggplot(data, aes(x = Volatility, y = ZAR_Rate)) +
        geom_point() +
        geom_smooth(method = "lm", color = "red", se = FALSE) +
        labs(title = "ZAR Rate vs Currency Implied Volatility",
             x = "Currency Implied Volatility", y = "ZAR Exchange Rate") +
        theme_minimal()


    plot4 <- ggplot(data, aes(x = Carry_Index, y = ZAR_Rate)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "ZAR Rate vs Carry Index",
             x = "Carry Index (DBHVG10U)", y = "ZAR Exchange Rate") +
        theme_minimal()


    grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
}
