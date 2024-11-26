

weights <- function(data_set, Ticker) {
    weights_data <- data_set %>%
        select(date, Tickers, Ticker) %>%
        spread(Tickers, Ticker) %>%
        tbl_xts()

    weights_data <- weights_data / rowSums(weights_data, na.rm = TRUE)
    weights_data[is.na(rowSums(weights_data, na.rm = TRUE)), ] <- 1 / ncol(weights_data)
    row_sums <- rowSums(weights_data, na.rm = TRUE)
    all_equal_to_one <- all(abs(row_sums - 1) < .Machine$double.eps)

    # Replace NA values only in numeric columns of data_set
    numeric_columns <- sapply(data_set, is.numeric)
    data_set[numeric_columns][is.na(data_set[numeric_columns])] <- 0

    weights_data[is.na(weights_data)] <- 1 / ncol(weights_data)

    return(weights_data)
}

