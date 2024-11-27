

create_plot <- function(data_global) {
    library(ggplot2)
    library(cowplot)
    data_global$quadrant <- with(data_global, ifelse(Return_cur < 0 & Global_Return > 0, "Top-Left",
                                                     ifelse(Return_cur < 0 & Global_Return < 0, "Bottom-Left",
                                                            ifelse(Return_cur > 0 & Global_Return > 0, "Top-Right",
                                                                   "Bottom-Right"))))

    percentages <- round(prop.table(table(data_global$quadrant)) * 100)

    x_min <- floor(min(data_global$Return_cur) * 100 / 5) * 5
    x_max <- ceiling(max(data_global$Return_cur) * 100 / 5) * 5
    y_min <- floor(min(data_global$Global_Return) * 100 / 5) * 5
    y_max <- ceiling(max(data_global$Global_Return) * 100 / 5) * 5

    p <- ggplot(data_global, aes(x = Return_cur * 100, y = Global_Return * 100)) +
        geom_point(alpha = 0.6) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        xlim(x_min, x_max) +
        ylim(y_min, y_max) +
        xlab("USD-ZAR Returns (%)") +
        ylab("60-40 Global Portfolio USD Returns (%)") +
        theme_bw() +
        theme(plot.margin = unit(c(1,1,2,1), "lines"))

    p <- p +
        annotate("rect", xmin = x_min, xmax = 0, ymin = y_min, ymax = y_max,
                 fill = "yellow", alpha = 0.1) +
        annotate("rect", xmin = 0, xmax = x_max, ymin = y_min, ymax = y_max,
                 fill = "green", alpha = 0.1)

    p <- p +
        annotate("text", x = x_min * 0.7, y = y_max * 0.9, label = "Hedge works but amplifies volatility\n(strong negative correlation)", hjust = 0, size = 3) +
        annotate("text", x = x_min * 0.7, y = y_min * 0.7, label = "Best case for hedge:\nhigher return, lower volatility", hjust = 0, size = 3) +
        annotate("text", x = x_max * 0.05, y = y_min * 0.7, label = "Hedge removes\ncurrency cushion", hjust = 0, size = 3) +
        annotate("text", x = x_max * 0.05, y = y_max * 0.9, label = "Hedge throws away returns", hjust = 0, size = 3) +
        annotate("text", x = 0, y = y_max * 1.05, label = "Hedge is not free", vjust = -0.5, hjust = 0.5, size = 4, fontface = "bold")

    quad_positions <- data.frame(
        quadrant = c("Top-Left", "Bottom-Left", "Top-Right", "Bottom-Right"),
        x = c(x_min * 0.5, x_min * 0.5, x_max * 0.5, x_max * 0.5),
        y = c(y_max * 0.5, y_min * 0.5, y_max * 0.5, y_min * 0.5),
        percentage = c(percentages["Top-Left"], percentages["Bottom-Left"], percentages["Top-Right"], percentages["Bottom-Right"])
    )

    p <- p +
        geom_text(data = quad_positions, aes(x = x, y = y, label = paste0(percentage, "%")), color = "black", size = 5, fontface = "bold")

    p <- p + geom_smooth(method = "lm", se = FALSE, color = "blue")

    p <- p +
        annotate("text", x = x_min, y = y_min - (y_max - y_min) * 0.1, label = "Source: Bloomberg. Calculation Satrix (31 December 2004 – 31 January 2023).", hjust = 0, size = 3)

    p_top <- ggplot(data_global, aes(x = Return_cur * 100)) +
        geom_density(fill = "green", alpha = 0.5) +
        theme_void() +
        xlim(x_min, x_max)

    p_right <- ggplot(data_global, aes(x = Global_Return * 100)) +
        geom_density(fill = "red", alpha = 0.5) +
        theme_void() +
        xlim(y_min, y_max) +
        coord_flip()

    p_top <- p_top + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    p_right <- p_right + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

    p_final <- plot_grid(p_top, NULL, p, ncol = 1, rel_heights = c(1, 0.1, 4))
    p_combined <- plot_grid(p_final, p_right, ncol = 2, rel_widths = c(4, 1))

    print(p_combined)
}
