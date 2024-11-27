
regression_summary <- function(model) {

    library(broom)
    library(kableExtra)


    tidy_results <- tidy(model)


    tidy_results <- tidy_results %>%
        mutate(significance = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01  ~ "**",
            p.value < 0.05  ~ "*",
            p.value < 0.1   ~ ".",
            TRUE            ~ ""
        ))


    html_table <- tidy_results %>%
        select(term, estimate, std.error, statistic, p.value, significance) %>%
        kbl(caption = "Regression Results", format = "html") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


    return(html_table)
}
