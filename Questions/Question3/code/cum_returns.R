
cum_returns <- function(return_data, weights_data1, weights_data2, weight_name1 = "ALSI", weight_name2 = "SWIX") {

    library(ggplot2)
    library(dplyr)
    library(tidyr)

    rtn_1 <- rmsfuns::Safe_Return.portfolio(R = return_data, weights = weights_data1) %>%
        xts_tbl() %>%
        rename(!!paste0("weighted_", weight_name1) := portfolio.returns) %>%
        mutate(!!paste0("rtn_", weight_name1) := cumprod(1 + !!sym(paste0("weighted_", weight_name1))))


    rtn_2 <- rmsfuns::Safe_Return.portfolio(R = return_data, weights = weights_data2) %>%
        xts_tbl() %>%
        rename(!!paste0("weighted_", weight_name2) := portfolio.returns) %>%
        mutate(!!paste0("rtn_", weight_name2) := cumprod(1 + !!sym(paste0("weighted_", weight_name2))))

    merged <- left_join(rtn_1, rtn_2, by = "date") %>%
        select(!!paste0("rtn_", weight_name1), !!paste0("rtn_", weight_name2), date) %>%
        gather(key = "return_type", value = "return_value", -date)


    plot <- ggplot(data = merged, aes(x = date, y = return_value, color = return_type, group = return_type)) +
        geom_line() +
        labs(title = "Line Chart of Returns",
             x = "Date",
             y = "Return Value",
             color = "Return Type") +
        theme_minimal() +
        fmxdat::theme_fmx()

    print(plot)
}
