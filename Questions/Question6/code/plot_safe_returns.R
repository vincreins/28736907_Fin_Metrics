plot_safe_returns <- function(rts, wts) {
    library(rmsfuns)
    library(dplyr)
    library(ggplot2)
    library(xts)

    S_ret <- rmsfuns::Safe_Return.portfolio(R = rts, weights = wts, lag_weights = TRUE) %>%
        xts_tbl() %>%
        rename(capped_ret = portfolio.returns)

    S_ret %>%
        mutate(S_ret = cumprod(1 + capped_ret)) %>%
        ggplot(aes(x = date, y = S_ret)) +
        geom_line(color = "steelblue") +
        labs(
            title = "Cumulative Returns with Quarterly Rebalancing and Caps",
            x = "Date",
            y = "Cumulative Index"
        ) +
        theme_minimal()
}
