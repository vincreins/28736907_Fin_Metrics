process_data <- function(cncy_Carry, bbdxy, ZAR_IV, ZAR) {
    data <- cncy_Carry %>%
        rename(Carry_Index = Price) %>%
        inner_join(bbdxy %>% rename(Dollar_Index = Price), by = "date") %>%
        inner_join(ZAR_IV %>% rename(Volatility = SouthAfrica_IV), by = "date") %>%
        inner_join(ZAR %>% rename(ZAR_Rate = SouthAfrica_Cncy), by = "date") %>%
        select(-Name.x, -Name.y)

    data <- data %>%
        mutate(
            Carry_Index_norm = (Carry_Index - mean(Carry_Index, na.rm = TRUE)) / sd(Carry_Index, na.rm = TRUE),
            Dollar_Index_norm = (Dollar_Index - mean(Dollar_Index, na.rm = TRUE)) / sd(Dollar_Index, na.rm = TRUE),
            Volatility_norm = (Volatility - mean(Volatility, na.rm = TRUE)) / sd(Volatility, na.rm = TRUE),
            ZAR_Rate_norm = (ZAR_Rate - mean(ZAR_Rate, na.rm = TRUE)) / sd(ZAR_Rate, na.rm = TRUE)
        )

    return(data)
}
