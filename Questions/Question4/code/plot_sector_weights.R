plot_sector_weights <- function(Port_Holds, BM_Holds) {
    library(ggplot2)
    library(dplyr)

    merged_data <- merge(Port_Holds, BM_Holds, by = c("Tickers", "date"), all = FALSE)

    sector_weights <- merged_data %>%
        group_by(date, Sector) %>%
        summarise(total_weight = sum(Weight, na.rm = TRUE)) %>%
        ungroup()

    ggplot(sector_weights, aes(x = date, y = total_weight, fill = Sector)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(
            title = "Total Weight per Sector by Date",
            x = "Date",
            y = "Total Weight"
        ) +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3")
}
