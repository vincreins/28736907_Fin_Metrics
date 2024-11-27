ALSI_index <- function(ALSI) {
    library(dplyr)
    library(tidyr)
    library(tbl2xts)

    ALSI_index <- ALSI %>%
        select(date, Return, Index_Name) %>%
        group_by(date, Index_Name) %>%
        summarise(Average_Return = mean(Return, na.rm = TRUE)) %>%
        arrange(date) %>%
        na.omit() %>%
        spread(Index_Name, Average_Return) %>%
        tbl_xts()

    return(ALSI_index)
}
