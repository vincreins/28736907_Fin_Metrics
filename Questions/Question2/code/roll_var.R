density_plot_var <- function(data, window_size = 36) {
    year_roll <- rollapply(data, window_size, var, by.column = TRUE, fill = NA)
    df_roll <- data.frame(date = index(year_roll), coredata(year_roll)) %>%
        gather(Funds, Variances, -date)
    means <- df_roll %>%
        group_by(Funds) %>%
        summarise(mean_variance = mean(Variances, na.rm = TRUE))

    ggplot(data = df_roll, aes(x = Variances, group = Funds, fill = Funds)) +
        geom_density(adjust = 1.5, alpha = 0.4) +
        geom_vline(data = means, aes(xintercept = mean_variance, color = Funds),
                   linetype = "dashed", size = 0.7) +
        theme_minimal() +
        labs(title = "Density Plot with Variances",
             x = "Variances",
             y = "Density") +
        theme(legend.position = "right")
}
