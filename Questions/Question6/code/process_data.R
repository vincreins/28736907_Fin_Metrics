process_data <- function(start_date, end_date, msci, MAA) {

    dates5 <- dateconverter(as.Date(start_date), as.Date(end_date), "weekdayEOM") %>%
        data.frame() %>%
        set_names("date")

    eom_msci <- merge(dates5, msci, by = "date")
    eom_MAA <- merge(dates5, MAA, by = "date")


    eom_MAA <- eom_MAA %>%
        mutate(
            Type = case_when(
                startsWith(Ticker, "L") ~ "Rates",
                startsWith(Ticker, "A") ~ "Currency",
                startsWith(Ticker, "D") ~ "Currency",
                startsWith(Ticker, "B") ~ "Commodity",
                TRUE ~ "Other"
            )
        ) %>%
        select(-Ticker)


    eom_msci <- eom_msci %>% mutate(Type = "Equity")

    merged <- rbind(eom_msci, eom_MAA)

    merged <- merged %>%
        group_by(Name) %>%
        filter(n() >= 36) %>%
        ungroup()

    return(merged)
}
