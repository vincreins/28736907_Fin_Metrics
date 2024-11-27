---
output:
  md_document:
    variant: markdown_github
---




```{r, eval = F}
fmxdat::make_project()
```



```{r}

# PDF Texevier:
Texevier::create_template(directory = "Questions",
                          template_name = "Question1"
)

# HTML Texevier:
Texevier::create_template_html(directory = "Questions",
                          template_name = "Question2"
)

# PDF Texevier:
Texevier::create_template(directory = "Questions",
                          template_name = "Question3"
)

# HTML Texevier:
Texevier::create_template_html(directory = "Questions",
                          template_name = "Question4"
)

# PDF Texevier:
Texevier::create_template(directory = "Questions",
                          template_name = "Question5"
)

# HTML Texevier:
Texevier::create_template_html(directory = "Questions",
                          template_name = "Question6"
)
```
Question 1

```{r}
ASISA <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question1/data/ASISA_Rets.rds")
BM <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question1/data/Capped_SWIX.rds")
AI_Funds <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question1/data/AI_Max_Fund.rds")
```


I am tasked to showcase the performance of the AI Implementer fund, comparing it against the benchmark (Capped SWIX) and industry peers (ASISA active managers). Using insights inspired by Bill Sharpe's work, I will illustrate how actively managed funds often struggle to outperform their benchmarks and the AI fund after fees. Employing a rolling period approach, I will highlight key performance metrics, emphasizing the systematic advantages of the AI fund through graphical representations and data-driven insights.

I will start my analysis using the PerfomanceAnalytics package, to make use of this package, it is necessary to spread the data. 
```{r}
AI_xts <- AI_Funds %>% tbl_xts()
BM_xts <- BM %>% select(-Tickers) %>% tbl_xts()
```

Since we also want to compare the AI Fund to managed funds, I created the function mean_A which calculates the returns across all funds for each date. 

```{r}
ASISA_mean <- mean_A(ASISA) 
```

The format of the three datasets now allows to merge them. Furthermore, the columns are renamed for clarity. The merged dataset can then be used to plot a cumulative return chart.

```{r}
merged <- merge.xts(AI_xts, ASISA_mean, BM_xts) %>% na.omit() 
colnames(merged) <- c("AI_Fund", "Managed_Funds", "Benchmark")

chart.CumReturns(merged, legend.loc = 1)
```

After calculating cumulative returns to assess overall performance, rolling returns are analyzed to evaluate consistency and resilience over different time periods, addressing potential biases from early outcomes or isolated events. 

```{r}
chart.RollingPerformance(merged, 12, legend.loc = 3)
```

The density_plot function creates a density plot to visualize and compare the distribution of rolling returns for multiple funds or data series, including the AI Implementer fund, the benchmark (Capped SWIX), and industry peers (ASISA active managers). It calculates rolling averages over a specified window size (window_size, default is 36) to smooth the data and better reflect trends over time. 

The function converts the rolling averages into a tidy data frame, calculates mean returns for each fund or series, and generates a density plot using ggplot2. The plot displays density curves for each fund, with vertical dashed lines marking the mean returns, and includes titles and labels for clarity.

This visualization helps fulfill the task's goal by demonstrating how the AI fund's return distribution compares to the benchmark and peers, emphasizing return consistency and central tendencies.

```{r}
density_plot(merged, 36)
```

The density_plot_var function creates a density plot to visualize and compare the distribution of rolling variances for multiple funds or data series over a specified window size (window_size, default is 36). It calculates rolling variances to measure the variability of returns over time, which reflects the risk profiles of the AI Implementer fund, the benchmark (Capped SWIX), and industry peers (ASISA active managers).

The function converts the rolling variances into a tidy data frame, computes mean variances for each fund, and generates a density plot using ggplot2. The plot displays density curves for the variances of each fund, with vertical dashed lines indicating the mean variances. Titles, labels, and themes enhance interpretability.

This plot helps illustrate the relative risk levels and variability patterns among the funds, complementing the return-focused analysis. By showcasing the AI fund's risk distribution compared to the benchmark and peers, it provides additional evidence supporting its systematic performance advantage, aligning with the task's goal to highlight the limitations of active management and the strengths of the AI fund.

```{r}
density_plot_var(merged, 36)
```


Calculating the downside deviation and analyzing drawdowns through these functions provides insights into risk management and the behavior of the AI Implementer fund compared to the benchmark (Capped SWIX) and industry peers (ASISA active managers).By calculating downside deviation, we can highlight how the AI fund manages negative outcomes relative to peers. A drawdown chart visually represents the peak-to-trough declines in value over time, offering an intuitive way to assess the severity and duration of losses.

```{r}
DownsideDeviation(merged, 0)
chart.Drawdown(merged, legend.loc = 3)
```

Question 2

This task replicates and expands on a study examining the impact of systematic currency hedging on a 60/40 equity and bond portfolio with a 70/30 local/global split. By comparing rolling volatilities for hedged and unhedged scenarios, I will highlight how the rand's negative correlation with global assets reduces portfolio volatility and enhances returns when left unhedged. Additional figures and tables will support the argument against long-term hedging, emphasizing its tactical, short-term value.

```{r}
Indexes <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question2/data/Cncy_Hedge_Assets.rds")
ZAR <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question2/data/Monthly_zar.rds")
```

I will start by calculating the currency returns, which will be needed for the replication of the graphic

```{r}
ZAR_YM <- ZAR %>% mutate(date = format(as.Date(date), "%Y-%m"))
USD_ZAR_returns <- ZAR_YM %>% mutate(Return_cur = value/lag(value) - 1)
```

In a next step the hedged and unhedged returns are calculated. The unhedged returns are calculated by the calculate_returns function, which computes unhedged portfolio returns by converting local indices (42% J433, 28% ALBI) to USD using exchange rates, combining them with global returns (18% MSCI ACWI, 12% Bloomberg Agg), and including USD/ZAR returns for analysis. hedged_returns calculates fully hedged portfolio returns using the same weights but retains local indices directly in ZAR, without exchange rate adjustments. The hedged_returns function calculates hedged portfolio returns using the same weights but keeps local indices in ZAR, excluding currency effects.

```{r}
Return <- calculate_returns(Indexes, ZAR_YM, USD_ZAR_returns)
Return_hedged <- hedged_returns(Indexes)
```

To compute the graph from the study, the unhedged returns and the exchange rate returns are merged to one data frame. This data frame is then used with the create_pot function.
The create_plot function generates a scatter plot with USD-ZAR returns (Return_cur) on the x-axis and global portfolio USD returns (Global_Return) on the y-axis, dividing data into quadrants based on the sign of the returns. It calculates and annotates quadrant percentages, adds descriptive annotations, shades specific areas, and overlays a trend line. Marginal density plots for USD-ZAR and portfolio returns are added on the top and right, respectively, and the plots are combined into a single layout with source information included.

```{r}

data_global <- Return %>% select(date, Global_Return, Return_cur) 
create_plot(data_global)
```

To further examine the performance and volatility differences between hedged and unhedged portfolios, I merge both data frames using the create_merged function; the function uses left_join to merge both sets by date.

```{r}
merged <- create_merged(Return, Return_hedged)

cur_ret <- USD_ZAR_returns %>%
    mutate(date = as.yearmon(date)) %>%
    tbl_xts() %>%
    .["2002-02/"] 
```

We can reuse the functions created in Question 1, which produce density plots for the returns and the variance, both for hedged and unhedged data. This can show the impact of hedging on the volatility and the return.

```{r}
density_plot(merged)
```

```{r}
density_plot_var(merged)
```

To gain further insights on the return differences of hedged and unhedged data the annualized returns will be calculated, this will present the geometric average return. To find out whether the results are consistent and not just driven by outliers, the rolling returns will be plotted.

```{r}
Return.annualized(merged, 12)
```

```{r}
chart.RollingPerformance(merged, legend.loc = 3)
```


Question 3


This Question explores the methodologies and performance differences between the ALSI (J203) and SWIX (J403) indexes. The analysis examines their size-based performance (large, mid, and small caps), sector exposures, and stock concentration trends over time, highlighting key distinctions in return profiles. Additionally, the report evaluates the impact of capping levels (5%, 10%, and uncapped) on both indexes, addressing the JSE's query regarding the application of capping. This study aims to provide actionable insights into how capping influences index characteristics and overall performance.

```{r}
ALSI <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question3/data/ALSI.rds")
RebDays <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question3/data/Rebalance_days.rds")
MonZar <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question3/data/Monthly_zar.rds")
```

To prepare the data for further analyzes, the data will be transformed to a wide format by returns by Tickers. To calculate further Portfolio performance, the weights of the respective Tickers will be extracted using the weight function. The weights function normalizes portfolio weights for each ticker in a dataset by selecting relevant columns, reshaping the data into a wide format, and dividing each value by the row sum to ensure weights in each row sum to 1. It handles missing values by assigning equal weights for rows with all NA values and replaces NAs in numeric columns with 0. The function validates that all row sums are equal to 1 and returns the adjusted weights as a time-series object.

```{r}
rtn <- ALSI %>% select(date, Tickers, Return) %>% spread(Tickers, Return) %>% tbl_xts()
weights_ALSI <- weights(ALSI, "J203")
weights_SWIX <- weights(ALSI, "J403")
```

The reshaping and calculations of the data in the previous step allow to calculate cumaltive returns, this is achieved by the cum_returns function. The cum_returns function calculates and visualizes cumulative returns for two portfolios based on their respective weights. It uses the Safe_Return.portfolio function to compute portfolio returns for the given weight sets, converts the returns to cumulative values, and merges the results into a tidy format. The cumulative returns are plotted as a line chart over time, with separate lines for each portfolio, labeled by their respective weight names (weight_name1 and weight_name2).

```{r}
cum_returns(return_data = rtn, 
                         weights_data1 = weights_ALSI, 
                         weights_data2 = weights_SWIX, 
                         weight_name1 = "ALSI", 
                         weight_name2 = "SWIX")
```

To take a deeper dive into the different Capitalizations, the rolling performance of the different capitalizations will be Analyzed. If big differences can be found, a performance difference can be traced back to that. To achieve this, the function ALSI_index will be applied. The ALSI_index function processes ALSI data to compute the average return for each index by date. It selects relevant columns (date, Return, Index_Name), groups the data by date and Index_Name, calculates the mean return, arranges the data by date, removes missing values, reshapes it into a wide format with Index_Name as columns, and converts the result into a time-series (xts) object.

```{r}
ALSI_index_ret <- ALSI_index(ALSI)
chart.RollingPerformance(ALSI_index_ret, width = 252, legend.loc = 1)
```

To identify differences in the sectors, the two Funds are active are Barchart will be plotted, with the sector share for each fund. This will be done using the sector_exp function. The sector_exp function calculates and visualizes the average sector exposure for the ALSI (J203) and SWIX (J403) methodologies. It groups the data by sector and date, computes the total weight for each methodology, reshapes the data into a long format, and calculates the mean weight for each sector-methodology combination. The results are plotted as a bar chart, showing average weights by sector for both methodologies, with separate bars for ALSI and SWIX.

```{r}
sector_exp(ALSI)
```

Another difference might be found in the Stock concentration, the share the n stocks with highest weight have in total portfolio. This allows conclusion about the diversification. This will be done using the stock_concentration function. The stock_concentration function calculates the contribution of the top n stocks (default 10) to the total weight in the ALSI (J203) and SWIX (J403) indexes for each date. It selects the top n stocks by weight for each index, calculates their combined contribution, merges the results by date, and plots a line chart to compare the stock concentration trends over time for both indexes.

```{r}
stock_concentration(ALSI, 10)
```

To answer the secon part of the question, on what impact capping has at different levels, the Capped_funds function was created. The Capped_funds function calculates and compares capped index values for specified Indexes (J203_cols) and weight cap (W_Cap) values. It filters rebalancing days, selects the top 30 stocks for each Index, normalizes the weights, and applies a proportional capping mechanism to ensure no stock exceeds the specified weight cap. The function computes portfolio returns, cumulative index values, and metadata for each combination of Indexes and weight cap. It outputs a dataset of results and a line plot comparing capped indexes across scenarios over time.

```{r}
Capped_funds(
  ALSI = ALSI, 
  RebDays = RebDays, 
  J203_cols = c("J203"), 
  W_Cap_values = c(0.05, 0.10, 1)
)
```

Question 4

I this question I will analyze the performance and positioning of SnakeOil Capital, a long-only domestic equity fund tracking the FTSE/JSE Capped SWIX. The analysis will cover relative risk (e.g., tracking error, active share, downside risk), relative performance (e.g., return comparisons, information ratio, rolling performance), fund positioning (e.g., stock and sector over/underweights), and performance attribution (e.g., drivers of relative and absolute performance, success of positioning decisions). The findings will be presented visually in a PowerPoint deck.

```{r}
Port_Holds <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question4/data/Fund_Holds.rds")
Port_Rets <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question4/data/Fund_Rets.rds")
BM_Holds <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question4/data/BM_Holds.rds")
BM_Rets <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question4/data/BM_Rets.rds")
Capped_SWIX <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question4/data/Capped_SWIX.rds")
ALSI <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question4/data/ALSI.rds")

```

To prepare the data for later analysis, the benchmark and the Snakeoil portfolio are capped to the same start and end date and transformed to xts format. 

```{r}
Swix <- BM_Rets %>% filter(date >= ymd(20191031))
Endate <- ymd(20241031)

Ra <- Swix %>%
    filter(date <= Endate) %>%
    filter(date >= fmxdat::safe_month_min(last(date), N = 36)) %>%
    tbl2xts::tbl_xts(cols_to_xts = BM)

Rs <- Port_Rets %>%
    filter(date <= Endate) %>%
    filter(date >= fmxdat::safe_month_min(last(date), N = 36)) %>%
    tbl2xts::tbl_xts(cols_to_xts = Returns)

pacman::p_load(tbl2xts, PerformanceAnalytics)

xts_port <- tbl_xts(Port_Rets)
xts_bench <- tbl_xts(BM_Rets)
```

The data then allows to calculate quantitative information on the fund. As a first step the tracking error is calculated which measures the standard deviation of the difference between a portfolio's returns and its benchmark's returns, indicating the consistency of the portfolio's performance relative to the benchmark.

```{r}
TE <- fmxdat::Safe_TE(Ra, Rs, scale = 12)
```

As a next step the cumulative returns of the Benchmark and the Snakeoil Fund will be plotted to give a first impression which one performs better and by how much.

```{r}
merged <- merge.xts(xts_port, xts_bench)
chart.CumReturns(merged, legend.loc = 1)
```

Then, the 12-month rolling beta is calculated to provide a dynamic view of the fund's sensitivity to the benchmark over time. This metric helps assess how the fund's positioning aligns with or deviates from the benchmark under different market conditions, offering insights into changes in risk exposure and informing discussions on relative performance and strategy effectiveness.

```{r}
chart.RollingRegression(Ra = xts_port, Rb = xts_bench, width = 12,
    attribute = c("Beta"))
```

To compare the cumulative returns of the fund against its benchmark, the chart.RelativePerformance function is used to visually provide a clear perspective on the fund's relative performance over time and highlighting periods of outperformance or underperformance.

```{r}
chart.RelativePerformance(xts_port, xts_bench)
```

To measure the fund's risk-adjusted performance relative to its benchmark, the Information Ratio is calculated, helping to evaluate the consistency and efficiency of the fund manager's active investment decisions.

```{r}
IR <- ratio.information(xts_port, xts_bench)
```

Inorder to provide insights into alpha, beta, R-squared, and other metrics that quantify the fund's relative performance and risk characteristics over time, the SFM table is computed.

```{r}
sfm <- table.SFM(Ra = xts_port, Rb = xts_bench, 12)
```


Question 5

I this question I will analyze the South African rand (ZAR) relative to the USD, comparing its volatility and performance to other major currencies. The analysis will measure volatility across time and compare the ZAR with G10 currencies to confirm if it is among the most volatile. Additionally, I will evaluate the ZAR's performance during favorable G10 currency carry trade periods and assess its behavior during phases of Dollar strength, indicating risk-on sentiment. Insights will be presented using tables, graphs, and statistical comparisons to support the conclusions.

```{r}
cncy <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question5/data/currencies.rds")
cncy_Carry <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question5/data/cncy_Carry.rds")
cncy_value <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question5/data/cncy_value.rds")
cncyIV <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question5/data/cncyIV.rds")
bbdxy <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question5/data/bbdxy.rds")

```

To prepare the data for further analysis, the currency data will be transformed to xts format. Since the data set contains everyday including weekends, where no trade data is available, the Weekends will be dropped from the data set. To enable an analyzes over a long period of time, and therefore many degrees of freedom,  all currencies which only had data available at a later point in time will be excluded from the analyzis. 

```{r}
xts_cncy <- cncy %>% spread(Name, Price) %>% tbl_xts()
index(xts_cncy) <- as.Date(index(xts_cncy))
xts_cncy <- xts_cncy[!weekdays(index(xts_cncy)) %in% c("Saturday", "Sunday"), ]
xts_cncy <- xts_cncy[, !apply(is.na(xts_cncy), 2, any)]
```

To get a first idea, which currencies are especially volatile, the currency_volatility function will be used. The currency_volatility function calculates the rolling volatility of the top 10 most volatile currencies in a time series dataset. It computes daily returns for each currency, identifies the top 10 currencies with the highest standard deviation of returns, and filters the dataset to include only these currencies. A 252-day rolling standard deviation is then applied to measure volatility trends over time, and the results are visualized in a time-series plot with a legend.

```{r}
currency_volatility(xts_cncy)
```

To also get an Idea, whether this aligns with the implied volatility by the markets, the same function will be applied to the cncyIV dataset.

```{r}
xts_IV <- cncyIV %>% spread(Name, Price) %>% tbl_xts() %>% .["1999-03-01/"] 
xts_IV <- xts_IV[, colSums(is.na(xts_IV)) == 0]

currency_volatility(xts_IV)
```

These results answer the question, whether the ZAR is one of the most volatile currencies. To investigate, whether the ZAR has performed well when G10 currency carry trades have been favourable, and whether it has benefited during periods with strong dollars, a regression model is build. To do this, in a first step the process_data function is applied. The process_data function merges multiple datasets containing the currency carry index, Dollar Index, ZAR implied volatility, and ZAR exchange rate based on the date column. It renames key columns for clarity, removes redundant columns, and calculates normalized values (z-scores) for each variable by subtracting the mean and dividing by the standard deviation. The function returns a cleaned and normalized dataset for further analysis.

```{r}
ZAR <- xts_cncy %>% xts_tbl() %>% select(date, SouthAfrica_Cncy)
ZAR_IV <- cncyIV %>% spread(Name, Price) %>% select(date, SouthAfrica_IV)

data <- process_data(cncy_Carry, bbdxy, ZAR_IV, ZAR)

model <- lm(ZAR_Rate_norm ~ Carry_Index_norm + Dollar_Index_norm + Volatility_norm, data = data)

regression_summary(model)
```

To visualize the results from the regression and substantiate it, the normalized data will plotted.

```{r}
plot_normalized(data)
```

Subsequently the plots function will be applied. The plots function generates a set of scatter plots to visualize the relationships between the ZAR exchange rate and key independent variables: the Carry Index, Dollar Index, and implied volatility. For each variable, a scatter plot is created with a linear regression line overlaid to indicate trends.
The first plot shows the relationship between the ZAR exchange rate and the Carry Index, with a blue regression line. The second plot examines the ZAR exchange rate against the Dollar Index, using a green regression line. The third plot explores the ZAR exchange rate and implied volatility, represented by a red regression line. The fourth plot repeats the ZAR vs. Carry Index relationship for comparison.
These plots are arranged into a grid with two columns using grid.arrange, providing a cohesive visual summary of the ZAR exchange rate's interactions with the specified variables.

```{r}
plots(data)
```

The plot_rolling_correlations provide a visual insight into the correlation results, and furthermore ensures, that the result is not driven by a few outliers. The plot_rolling_correlations function calculates and visualizes the rolling correlations between the ZAR exchange rate and three predictors: the Carry Index, Dollar Index, and implied volatility. It uses a specified rolling window (default: 252 days) to compute correlations over time.

The function first calculates rolling correlations for each predictor using rollapply, aligning results to the right and filling gaps with NA values. It then generates a line plot with separate lines for each predictor, showing how their correlations with the ZAR exchange rate evolve over time. The output is a list containing the augmented dataset with calculated correlations and the correlation plot.

```{r}
plot_rolling_correlations(data, 252)
```

Question 6

In this question, I will construct a Global Balanced Index Fund portfolio using traded global indexes. The analysis will incorporate a long-only strategy with monthly data post-2010, focusing on risk and return optimization using a covariance matrix and mean forecasts based on a look-back period of less than three years. The portfolio will adhere to constraints on asset exposure, including limits of 60% for equities, 25% for bonds and credit instruments, and 40% for single assets, with quarterly rebalancing applied. The goal is to design a balanced, diversified portfolio while exploring various analytical perspectives and methodologies.

```{r}
MAA <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question6/data/MAA.rds")
msci <- read_rds("/Users/x/Downloads/28736907_Fin_Metrics/Questions/Question6/data/msci.rds") %>%
filter(Name %in% c("MSCI_ACWI", "MSCI_USA", "MSCI_RE", "MSCI_Jap"))
```

To prepare the data, the function process_data is applied. The function processes and merges msci and MAA datasets, aligning them to end-of-month dates. It categorizes MAA data by asset type based on tickers, assigns "Equity" to msci data, combines both datasets, and filters out assets with less than 36 months of data. The result is a cleaned, classified, and aligned dataset for analysis.

```{r}
merged <- process_data("2011-01-01", "2023-01-01", msci, MAA)
```

Subsequently the log returns are calculated by the calculate_returns function.

```{r}
returns_data <- calculate_returns(merged)
```

And the rebalance days are determined by the reb_days function.

```{r}
Rebalance_Days <- reb_days(returns_data)

```

Now the reb_data function is applied, which processes returns_data to calculate portfolio weights on specified rebalance days. It filters data for dates in Rebalance_Days, groups it by rebalance periods (RebalanceTime), sorts assets by descending price, calculates weights as a proportion of total price for each period, and organizes the results chronologically. The output is a dataset with calculated weights for each rebalance period.

```{r}
rebalance_col <- reb_data(returns_data, Rebalance_Days)
```

As the datasets are now prepared, it is possible to create the adjusted dataset with capped weights according to the speifications. This is done with the Cap_With_Groups function. It adjusts portfolio weights in a dataframe by applying caps based on asset type and ensures the weights remain properly normalized. It first verifies that the necessary columns (weight and Type) are present. Next, it assigns a cap (group_cap) to each asset based on its type: equities are capped by Equity_Cap, bonds by Bond_Cap, and other assets by W_Cap. It then checks if the total weight of any group exceeds its cap, and if so, proportionally scales down the weights in that group to meet the cap. Afterward, it ensures no individual weight exceeds W_Cap and normalizes all weights to sum to 1. Finally, it validates that the adjusted weights sum to 1 and returns the updated dataframe with capped and normalized weights.

```{r}
Capped_df <- rebalance_col %>%
  group_split(RebalanceTime) %>%
  map_df(~Cap_With_Groups(., W_Cap = 0.4, Equity_Cap = 0.6, Bond_Cap = 0.25)) %>%
  select(-RebalanceTime)
```

To calculate the Return of the portfolio the weights and the returns have to be spread.

```{r}
wts <- Capped_df %>%
  tbl_xts(cols_to_xts = weight, spread_by = Name)

rts <- returns_data %>%
  filter(Name %in% unique(Capped_df$Name)) %>%
  tbl_xts(cols_to_xts = Return, spread_by = Name)


wts[is.na(wts)] <- 0
rts[is.na(rts)] <- 0
```

To plot the cumulative returns, the plot_safe_returns is applied. It calculates and visualizes the cumulative returns of a portfolio with quarterly rebalancing and capped weights. It takes return data (rts) and weight data (wts) as inputs. First, the function computes the portfolio returns using Safe_Return.portfolio from the rmsfuns package, with lagged weights to account for rebalancing. The resulting returns are renamed to capped_ret and converted into cumulative returns by compounding over time (cumprod). The cumulative returns are then plotted using ggplot2 as a time series, with a line representing the portfolio's growth over time.



