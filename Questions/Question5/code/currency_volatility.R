currency_volatility <- function(xts_cncy) {
    library(dplyr)
    library(zoo)
    library(xts)

    returns <- xts_cncy %>%
        { . / lag(., 1) - 1 } %>%
        na.omit()

    volatility <- apply(returns, 2, sd)
    top_volatility <- sort(volatility, decreasing = TRUE)[1:10] %>% as.data.frame()
    top_currencies <- rownames(top_volatility)

    filtered_cncy <- returns[, top_currencies, drop = FALSE]
    roll_cncy <- rollapply(filtered_cncy, 252, sd) %>% na.omit()

    plot.xts(roll_cncy, legend.loc = 5)

}
