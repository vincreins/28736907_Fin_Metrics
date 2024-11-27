create_merged <- function(Return, Return_hedged) {
    library(dplyr)
    library(zoo)
    library(tbl2xts)

    unhedged <- Return %>%
        select(date, Portfolio_Return) %>%
        mutate(date = as.yearmon(date))

    hedged <- Return_hedged %>%
        select(date, Portfolio_Return_Hedged) %>%
        mutate(date = as.yearmon(date))

    merged <- left_join(unhedged, hedged, by = "date") %>%
        tbl_xts()

    return(merged)
}
