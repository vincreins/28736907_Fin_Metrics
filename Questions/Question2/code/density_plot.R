density_plot <- function(data, window_size = 36) {
    library(RcppRoll)
    year_roll <- roll_mean(data, window_size)
    df_roll <- data.frame(date = index(year_roll), coredata(year_roll)) %>%
        gather(Funds, Returns, -date)
    means <- df_roll %>%
        group_by(Funds) %>%
        summarise(mean_return = mean(Returns, na.rm = TRUE))

    ggplot(data = df_roll, aes(x = Returns, group = Funds, fill = Funds)) +
        geom_density(adjust = 1.5, alpha = 0.4) +
        geom_vline(data = means, aes(xintercept = mean_return, color = Funds),
                   linetype = "dashed", size = 0.7) +
        theme_minimal() +
        labs(title = "Density Plot with Means",
             x = "Returns",
             y = "Density") +
        theme(legend.position = "right")
}
