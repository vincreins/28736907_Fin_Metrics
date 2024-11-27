Cap_With_Groups <- function(df_Cons, W_Cap = 0.4, Equity_Cap = 0.6, Bond_Cap = 0.25) {
    if (!"weight" %in% names(df_Cons)) stop("Column 'weight' is required.")
    if (!"Type" %in% names(df_Cons)) stop("Column 'Type' is required.")


    df_Cons <- df_Cons %>%
        mutate(
            group_cap = case_when(
                Type == "Equity" ~ Equity_Cap,
                Type == "Bond" ~ Bond_Cap,
                TRUE ~ W_Cap
            )
        )


    for (group in unique(df_Cons$Type)) {
        group_total <- sum(df_Cons %>% filter(Type == group) %>% pull(weight))
        if (group_total > df_Cons$group_cap[1]) {
            scaling_factor <- df_Cons$group_cap[1] / group_total
            df_Cons <- df_Cons %>%
                mutate(weight = ifelse(
                    Type == group,
                    weight * scaling_factor,
                    weight
                ))
        }
    }


    df_Cons <- df_Cons %>%
        mutate(weight = pmin(weight, W_Cap))


    df_Cons <- df_Cons %>%
        mutate(weight = weight / sum(weight))


    if (abs(sum(df_Cons$weight) - 1) > 0.001) {
        stop("Weights do not sum to 1 after capping.")
    }

    df_Cons
}
