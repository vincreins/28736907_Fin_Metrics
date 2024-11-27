Capped_funds <- function(ALSI, RebDays, J203_cols, W_Cap_values) {
  # Helper function for a single J203_col and W_Cap
  calculate_index <- function(ALSI, RebDays, J203_col, W_Cap) {
    J203_col <- as.character(J203_col)
    # Step 1: Filter rebalance trade days
    reb_trade_days <- RebDays %>%
      filter(Date_Type == "Reb Trade Day") %>%
      select(date)
    
    # Step 2: Calculate rebalance weights
    rebalance_col <- ALSI %>%
      filter(date %in% reb_trade_days$date) %>%
      group_by(date) %>%
      arrange(desc(!!sym(J203_col))) %>%
      top_n(30, !!sym(J203_col)) %>%
      mutate(weight = !!sym(J203_col) / sum(!!sym(J203_col))) %>%
      ungroup() %>%
      arrange(date) %>%
      select(-Sector, -Return, -J203, -J403, -Index_Name) # Adjust based on columns available
    
    # Step 3: Apply proportional capping
    Proportional_Cap_Foo <- function(df_Cons, W_Cap = 0.05) {
      Breachers <- df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers)
      if (length(Breachers) > 0) {
        while (df_Cons %>% filter(weight > W_Cap) %>% nrow() > 0) {
          df_Cons <- bind_rows(
            df_Cons %>% filter(Tickers %in% Breachers) %>% mutate(weight = W_Cap),
            df_Cons %>% filter(!Tickers %in% Breachers) %>%
              mutate(weight = (weight / sum(weight, na.rm = T)) * (1 - length(Breachers) * W_Cap))
          )
          Breachers <- c(Breachers, df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers))
        }
      }
      df_Cons
    }
    
    Capped_df <- rebalance_col %>%
      group_split(date) %>%
      map_df(~Proportional_Cap_Foo(., W_Cap = W_Cap))
    
    # Step 4: Create weight and return matrices
    wts <- Capped_df %>%
      tbl_xts(cols_to_xts = weight, spread_by = Tickers)
    rts <- ALSI %>%
      filter(Tickers %in% unique(Capped_df$Tickers)) %>%
      tbl_xts(cols_to_xts = Return, spread_by = Tickers)
    
    wts[is.na(wts)] <- 0
    rts[is.na(rts)] <- 0
    
    # Step 5: Calculate the portfolio index
    Idx <- rmsfuns::Safe_Return.portfolio(R = rts, weights = wts, lag_weights = TRUE) %>%
      xts_tbl() %>%
      rename(capped_Idx = portfolio.returns) %>%
      mutate(Idx = cumprod(1 + capped_Idx)) %>%
      mutate(J203_col = J203_col, W_Cap = W_Cap) # Add identifiers for plotting
    
    Idx
  }
  
  # Apply the function to each combination of J203_col and W_Cap
  results <- expand.grid(J203_col = J203_cols, W_Cap = W_Cap_values) %>%
    pmap_dfr(~calculate_index(ALSI, RebDays, ..1, ..2))
  
  # Plot all results in one graph
  plot <- results %>%
    ggplot(aes(x = date, y = Idx, color = interaction(J203_col, W_Cap, sep = " | "))) +
    geom_line() +
    labs(
      title = "Capped Index Comparison",
      subtitle = "Different J203_col and W_Cap values",
      x = "Date", y = "Index Value",
      color = "J203_col | W_Cap"
    ) +
    fmxdat::theme_fmx()
  
  list(Results = results, Plot = plot)
}
