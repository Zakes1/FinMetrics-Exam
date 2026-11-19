# FinMetrics-Exam
Financial econometrics exam 2026

# Question 1A

## Setup

These are all the packages that will be used in this section along with
loading the necessary datasets

``` r
pacman::p_load("tidyverse", "lubridate", "tbl2xts", "RcppRoll", "TTR", "PerformanceAnalytics", "glue")

pacman::p_install_gh("Nicktz/fmxdat")
```

    ## Skipping install of 'fmxdat' from a github remote, the SHA1 (6341356b) has not changed since last install.
    ##   Use `force = TRUE` to force installation

    ## 
    ## The following packages were installed:
    ## fmxdat

``` r
# Economic policy uncertainty indexes:
Uncertainty_Indexes <- "data/Econ_Uncertainty_Indexes.rds" %>% read_rds()

# Implied volatility indexes:
Implied_Vol <- "data/Implied_Vol_Indexes.rds" %>% read_rds()

# Index Constituents:
Local_Index_Constituents <- "data/Index_Constituents.rds" %>% read_rds()

# Index Daily Returns:
Idx_Daily <- read_rds("data/Index_Returns_Daily.rds")

# Currency Vol Indexes:
Local_Cncy_Vol_Index <- read_rds("data/LCL_Currency_Vol_Index.rds")
Gl_Cncy_Vol_Index <- read_rds("data/Global_Currency_Vol_Index.rds")

Commods <- read_rds("data/Commodities.rds")

MSCI      <- read_rds("data/MSCI_Indexes.rds")        
Glob_USD  <- read_rds("data/Global_Indexes_USD.rds")
```

Check what data I have in the datasets

``` r
list(Uncertainty_Indexes, Implied_Vol, Idx_Daily, Local_Cncy_Vol_Index, Gl_Cncy_Vol_Index, Commods, Local_Index_Constituents, MSCI, Glob_USD) %>% walk(glimpse)
```

    ## Rows: 31,768
    ## Columns: 6
    ## $ date     <date> 2005-01-02, 2005-01-03, 2005-01-04, 2005-01-05, 2005-01-06, …
    ## $ Ticker   <chr> "EPUCCEUM Index", "EPUCCEUM Index", "EPUCCEUM Index", "EPUCCE…
    ## $ TRI      <dbl> 83.33, 83.33, 83.33, 83.33, 83.33, 83.33, 83.33, 83.33, 83.33…
    ## $ Name     <chr> "EPUCCEUM", "EPUCCEUM", "EPUCCEUM", "EPUCCEUM", "EPUCCEUM", "…
    ## $ Group    <chr> "Econ_Unc", "Econ_Unc", "Econ_Unc", "Econ_Unc", "Econ_Unc", "…
    ## $ Currency <chr> "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD"…
    ## Rows: 15,402
    ## Columns: 6
    ## $ date     <date> 2005-01-03, 2005-01-04, 2005-01-05, 2005-01-06, 2005-01-07, …
    ## $ Ticker   <chr> "V2X Index", "V2X Index", "V2X Index", "V2X Index", "V2X Inde…
    ## $ TRI      <dbl> 19.50556, 19.30265, 19.15264, 18.19116, 17.68442, 16.91228, 1…
    ## $ Name     <chr> "V2X", "V2X", "V2X", "V2X", "V2X", "V2X", "V2X", "V2X", "V2X"…
    ## $ Group    <chr> "IV", "IV", "IV", "IV", "IV", "IV", "IV", "IV", "IV", "IV", "…
    ## $ Currency <chr> "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD"…
    ## Rows: 16,940
    ## Columns: 4
    ## $ date    <date> 2015-01-02, 2015-01-05, 2015-01-06, 2015-01-07, 2015-01-08, 2…
    ## $ Returns <dbl> -0.004836746, -0.030113156, 0.017807820, 0.009464806, 0.012172…
    ## $ Tickers <chr> "J433", "J433", "J433", "J433", "J433", "J433", "J433", "J433"…
    ## $ Name    <chr> "Capped SWIX", "Capped SWIX", "Capped SWIX", "Capped SWIX", "C…
    ## Rows: 3,396
    ## Columns: 7
    ## $ date        <date> 2013-09-25, 2013-09-26, 2013-09-27, 2013-09-30, 2013-10-0…
    ## $ Ticker      <chr> "SAVIUSD Index", "SAVIUSD Index", "SAVIUSD Index", "SAVIUS…
    ## $ TRI         <dbl> 14.56, 14.56, 15.45, 15.55, 15.56, 15.69, 15.39, 14.73, 15…
    ## $ Name        <chr> "SAVIUSD", "SAVIUSD", "SAVIUSD", "SAVIUSD", "SAVIUSD", "SA…
    ## $ Group       <chr> "Curncy_Vol", "Curncy_Vol", "Curncy_Vol", "Curncy_Vol", "C…
    ## $ Currency    <chr> "Local", "Local", "Local", "Local", "Local", "Local", "Loc…
    ## $ Description <chr> "Local Currency Volatility Index: The SaviUSD is specifica…
    ## Rows: 5,673
    ## Columns: 6
    ## $ date     <date> 2005-01-03, 2005-01-04, 2005-01-05, 2005-01-06, 2005-01-07, …
    ## $ Ticker   <chr> "JPMVXYGL Index", "JPMVXYGL Index", "JPMVXYGL Index", "JPMVXY…
    ## $ TRI      <dbl> 10.25, 10.31, 10.29, 10.29, 10.03, 9.94, 9.65, 9.89, 9.77, 9.…
    ## $ Name     <chr> "JPMVXYGL", "JPMVXYGL", "JPMVXYGL", "JPMVXYGL", "JPMVXYGL", "…
    ## $ Group    <chr> "Curncy_Vol", "Curncy_Vol", "Curncy_Vol", "Curncy_Vol", "Curn…
    ## $ Currency <chr> "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD"…
    ## Rows: 17,019
    ## Columns: 6
    ## $ date     <date> 2005-01-03, 2005-01-04, 2005-01-05, 2005-01-06, 2005-01-07, …
    ## $ Ticker   <chr> "BCOM Index", "BCOM Index", "BCOM Index", "BCOM Index", "BCOM…
    ## $ TRI      <dbl> 143.2955, 142.3930, 142.1799, 144.4881, 144.2124, 144.5722, 1…
    ## $ Name     <chr> "Bcom_Index", "Bcom_Index", "Bcom_Index", "Bcom_Index", "Bcom…
    ## $ Group    <chr> "Commodities", "Commodities", "Commodities", "Commodities", "…
    ## $ Currency <chr> "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD"…
    ## Rows: 655,336
    ## Columns: 6
    ## $ date        <date> 2020-08-31, 2020-08-31, 2020-08-31, 2020-08-31, 2020-08-3…
    ## $ Tickers     <chr> "ABG", "AGL", "ANG", "BID", "BTI", "CFR", "CLS", "CPI", "E…
    ## $ Return      <dbl> -0.03954, -0.00401, -0.00756, -0.03182, 0.01911, 0.01991, …
    ## $ weight      <dbl> 0.011121198, 0.040941454, 0.034772782, 0.038428999, 0.0077…
    ## $ SuperSector <chr> "Financials", "Resources", "Resources", "Industrials", "In…
    ## $ Index       <chr> "LowVol", "LowVol", "LowVol", "LowVol", "LowVol", "LowVol"…
    ## Rows: 82,900
    ## Columns: 6
    ## $ date     <date> 2005-01-03, 2005-01-04, 2005-01-05, 2005-01-06, 2005-01-07, …
    ## $ Ticker   <chr> "MXCN Index", "MXCN Index", "MXCN Index", "MXCN Index", "MXCN…
    ## $ TRI      <dbl> 4.1745, 4.0983, 3.9794, 3.9487, 3.9195, 3.9292, 3.9216, 3.946…
    ## $ Name     <chr> "MSCI China", "MSCI China", "MSCI China", "MSCI China", "MSCI…
    ## $ Group    <chr> "MSCI", "MSCI", "MSCI", "MSCI", "MSCI", "MSCI", "MSCI", "MSCI…
    ## $ Currency <chr> "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD"…
    ## Rows: 107,810
    ## Columns: 6
    ## $ date     <date> 1999-01-04, 1999-01-05, 1999-01-06, 1999-01-07, 1999-01-08, …
    ## $ Ticker   <chr> "CAC Index", "CAC Index", "CAC Index", "CAC Index", "CAC Inde…
    ## $ TRI      <dbl> 4902.760, 4942.638, 4989.292, 4942.269, 4912.800, 4847.732, 4…
    ## $ Name     <chr> "CAC_40_Index", "CAC_40_Index", "CAC_40_Index", "CAC_40_Index…
    ## $ Group    <chr> "Equities", "Equities", "Equities", "Equities", "Equities", "…
    ## $ Currency <chr> "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD"…

A detailed check of which Indexes I have and over what period

``` r
Idx_Daily %>% 
    group_by(Tickers) %>% 
    summarise(Start = min(date), End = max(date), N = n()) %>% 
    arrange(Start) %>% print(n = 50)
```

    ## # A tibble: 7 × 4
    ##   Tickers    Start      End            N
    ##   <chr>      <date>     <date>     <int>
    ## 1 J200       2015-01-02 2026-09-30  2935
    ## 2 J201       2015-01-02 2026-09-30  2935
    ## 3 J202       2015-01-02 2026-09-30  2935
    ## 4 J433       2015-01-02 2026-09-30  2935
    ## 5 STXMOMJ433 2017-03-23 2026-09-30  2379
    ## 6 STXVALJ433 2018-03-15 2026-09-30  2134
    ## 7 STXLVLJ433 2024-01-02 2026-09-30   687

Check my global indexes in the same way

``` r
MSCI %>% 
    group_by(Ticker) %>% 
    summarise(Start = min(date), End = max(date), N = n()) %>% 
    arrange(Start) %>% print(n = 50)
```

    ## # A tibble: 15 × 4
    ##    Ticker         Start      End            N
    ##    <chr>          <date>     <date>     <int>
    ##  1 MXCN Index     2005-01-03 2026-09-30  5673
    ##  2 MXEF Index     2005-01-03 2026-09-30  5673
    ##  3 MXEU Index     2005-01-03 2026-09-30  5673
    ##  4 MXIN Index     2005-01-03 2026-09-30  5673
    ##  5 MXJP Index     2005-01-03 2026-09-30  5673
    ##  6 MXUS Index     2005-01-03 2026-09-30  5673
    ##  7 MXWD Index     2005-01-03 2026-09-30  5673
    ##  8 MXWO Index     2005-01-03 2026-09-30  5673
    ##  9 MXWO000G Index 2005-01-03 2026-09-30  5673
    ## 10 MXWO000V Index 2005-01-03 2026-09-30  5673
    ## 11 MXWOMOM Index  2005-01-03 2026-09-30  5673
    ## 12 MXWOMVOL Index 2005-01-03 2026-09-30  5673
    ## 13 MXWOSM Index   2005-01-03 2026-09-30  5673
    ## 14 RMZ Index      2005-06-17 2026-09-30  5554
    ## 15 MXWOQU Index   2012-12-18 2026-09-30  3597

``` r
Glob_USD %>% 
    group_by(Ticker) %>% 
    summarise(Start = min(date), End = max(date), N = n()) %>% 
    arrange(Start) %>% print(n = 50)
```

    ## # A tibble: 14 × 4
    ##    Ticker         Start      End            N
    ##    <chr>          <date>     <date>     <int>
    ##  1 RUGL Index     1990-01-01 2026-09-30  9588
    ##  2 NDX Index      1990-01-02 2026-09-30  9587
    ##  3 SPXT Index     1990-01-02 2026-09-30  9587
    ##  4 UKX Index      1990-01-02 2026-09-30  9587
    ##  5 NKY Index      1990-01-04 2026-09-30  9585
    ##  6 JPEIGLBL INDEX 1993-12-31 2026-09-30  8544
    ##  7 MXWO0RE Index  1994-12-30 2026-09-30  8284
    ##  8 CAC Index      1999-01-04 2026-09-30  7238
    ##  9 CDAX Index     1999-01-04 2026-09-30  7238
    ## 10 SX5E Index     1999-01-04 2026-09-30  7238
    ## 11 FGCIITU Index  2005-12-29 2026-09-30  5415
    ## 12 MIFUREIT Index 2006-05-01 2026-09-30  5328
    ## 13 MIRUREIT Index 2006-05-01 2026-09-30  5328
    ## 14 TENGDNU Index  2006-07-31 2026-09-30  5263

Making my index choices for both local and global

``` r
# Capped Swix and Top 40
local_idx <- c("J433", "J200")

global_idx <- c("MXWO Index", "SPXT Index")
```

First I combine the convert global index from TRI to returns and create
a global index dataset that will be combined with the local dataset

``` r
Glob_USD_rets <- Glob_USD %>% 
    filter(Ticker %in% global_idx) %>% 
    group_by(Ticker) %>% 
    mutate(Tickers = Ticker) %>% 
    arrange(date) %>% 
    mutate(Returns = TRI / lag(TRI) - 1) %>% 
    ungroup() %>% 
    select(date, Tickers, Returns)

MSCI_rets <- MSCI %>% 
    filter(Ticker %in% global_idx) %>% 
    group_by(Ticker) %>% 
    mutate(Tickers = Ticker) %>% 
    arrange(date) %>% 
    mutate(Returns = TRI / lag(TRI) - 1) %>% 
    ungroup() %>% 
    select(date, Tickers, Returns)
```

Then I extract the same variables from my local dataset using the local
indexes I chose.

``` r
Local_rets <- Idx_Daily %>% 
    filter(Tickers %in% local_idx) %>% 
    select(date, Tickers, Returns)
```

Finally I combine local returns dataframe and global returns dataframe
and find the log returns

``` r
Rets <- bind_rows(Local_rets, Glob_USD_rets, MSCI_rets) %>% 
    arrange(date) %>% 
    mutate(LogRet = log(1 + Returns)) %>% 
    filter(!is.na(LogRet)) %>% 
    mutate(Region = ifelse(Tickers %in% local_idx, "Local", "Global"))
```

Now we set a common start date so that the local and global comparison
is fair

``` r
common_start <-  Rets %>% 
    group_by(Tickers) %>% 
    summarise(S = min(date)) %>% 
    pull(S) %>% 
    max()
Rets <- Rets %>% 
    filter(date>= common_start)
```

## Rolling 3 month and 1 year volatility

Now that the setup of the data is complete, I can now find the rolling
3-month and 1-year volatility

we have the 63 days (3 months, reactive), the 252 days (1 year, trend),
times sqrt(252) to annualise them.

``` r
RollVol <- Rets %>% 
    group_by(Tickers) %>% 
    arrange(date) %>% 
    mutate(RV_3m = RcppRoll::roll_sd(LogRet, 63, fill=NA, align = "right") * sqrt(252), 
           RV_1y = RcppRoll::roll_sd(LogRet, 252, fill = NA, align = "right") * sqrt(252)) %>% 
    ungroup()

g1 <- RollVol %>% 
    select(date, Tickers, Region, `3-month` = RV_3m, `1-year` = RV_1y) %>% 
    gather(Window, RV, `3-month`, `1-year`) %>% 
    filter(!is.na(RV)) %>% 
    ggplot() + 
    geom_line(aes(date, RV, colour = Tickers), alpha = 0.8, linewidth = 0.8) +
    facet_wrap(~Window, ncol = 1, scale = "free_y") + 
    fmxdat::theme_fmx() +
    fmxdat::fmx_cols() + 
    labs(x = "", y = "Annualised volatility", 
         title = "Rolling realised volatility: local vs global",
         caption = "Rolling SD of daily log returns x sqrt(252). Global index in USD.")
```

    ## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the fmxdat package.
    ##   Please report the issue at <https://github.com/nicktz/fmxdat/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the fmxdat package.
    ##   Please report the issue at <https://github.com/nicktz/fmxdat/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
g1_final = fmxdat::finplot(g1, y.pct = T)

dir.create("Figures", showWarnings = FALSE)
ggsave("Figures/Q1A_FIg1_RollingVol.pdf", plot = g1_final, width = 8, height = 5, dpi = 300)
plot(g1_final)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

## Annualised volatility and share of days with moves over 2%

now I will look at annualised volatility for pre-2020, Covid, 2021-23
and 2024 onwards, for local and global as well as the share of big days
as a percentage of days with moves larger than 2% each year to measure
tail risk.

``` r
Rets <- Rets %>% 
    mutate(Period = case_when(date < as.Date("2020-01-01") ~ "1. Pre-2020",
                              date < as.Date("2021-01-01") ~ "2. Covid (2020)",
                              date < as.Date("2024-01-01") ~ "3. 2021-2023",
                              TRUE ~ "4. 2024 onwards"))

Period_Tbl <- Rets %>% 
    group_by(Tickers, Region, Period) %>% 
    summarise(AnnVol = sd(LogRet) * sqrt(252), 
              Pct_Big_Days = mean(abs(LogRet) > 0.02)) %>% 
    ungroup()
```

    ## `summarise()` has grouped output by 'Tickers', 'Region'. You can override using
    ## the `.groups` argument.

``` r
# two wide tables for report
t1 <- Period_Tbl %>% 
    select(Tickers, Period, AnnVol) %>% 
    spread(Period, AnnVol)
t1
```

    ## # A tibble: 4 × 5
    ##   Tickers    `1. Pre-2020` `2. Covid (2020)` `3. 2021-2023` `4. 2024 onwards`
    ##   <chr>              <dbl>             <dbl>          <dbl>             <dbl>
    ## 1 J200               0.163             0.304          0.190             0.170
    ## 2 J433               0.160             0.308          0.166             0.160
    ## 3 MXWO Index         0.112             0.289          0.151             0.126
    ## 4 SPXT Index         0.132             0.341          0.173             0.149

``` r
t2 <- Period_Tbl %>% 
    select(Tickers, Period, Pct_Big_Days) %>% 
    spread(Period, Pct_Big_Days)
t2
```

    ## # A tibble: 4 × 5
    ##   Tickers    `1. Pre-2020` `2. Covid (2020)` `3. 2021-2023` `4. 2024 onwards`
    ##   <chr>              <dbl>             <dbl>          <dbl>             <dbl>
    ## 1 J200              0.0553             0.175         0.0963            0.0553
    ## 2 J433              0.0521             0.171         0.0682            0.0422
    ## 3 MXWO Index        0.0184             0.153         0.0461            0.0153
    ## 4 SPXT Index        0.0353             0.164         0.0704            0.0334

``` r
# Calendar-year view
Yearly_Vol <- Rets %>% 
    mutate(Year = year(date)) %>% 
    group_by(Tickers, Year) %>% 
    summarise(AnnVol = sd(LogRet) * sqrt(252)) %>% 
    ungroup()
```

    ## `summarise()` has grouped output by 'Tickers'. You can override using the
    ## `.groups` argument.

``` r
g2 <- Yearly_Vol %>% 
    ggplot() +
    geom_col(aes(as.factor(Year), AnnVol, fill = Tickers), position = "dodge") +
    fmxdat::theme_fmx() +
    fmxdat::fmx_fills() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "", y = "Annualised volatility", title = "Calendar-year realised volatility")

ggsave("Figures/Q1A_FIg2_YearlyAnnVol.pdf", plot = g2, width = 8, height = 5, dpi = 300)
plot(g2)
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

## Local to global volatility ratio

This is to check whether local volatility and global volatility move
together. This checks if local and global volatilities spike at the same
time.

``` r
local_main <- local_idx[1]
global_main <- global_idx[1]

RV_Ratio <- RollVol %>% 
    filter(Tickers %in% c(local_main, global_main)) %>% 
    select(date, Tickers, RV_1y) %>% 
    spread(Tickers, RV_1y) %>% 
    filter(!is.na(.data[[local_main]]), !is.na(.data[[global_main]])) %>% 
    mutate(Ratio = .data[[local_main]] / .data[[global_main]])

g3 <- RV_Ratio %>% 
    ggplot() +
    geom_line(aes(date, Ratio), colour = "steelblue", linewidth = 1) + 
    geom_hline(yintercept = 1, linetype = "dashed") +
    fmxdat::theme_fmx() +
    labs(x = "", y = "Ratio", 
         title = glue("Roling 1-year realised vol ratio: {local_main} / {global_main}"), 
         subtitle = "Above 1 = local more volatile than global")
ggsave("Figures/Q1A_FIg3_VolRatio.pdf", plot = g3, width = 8, height = 5, dpi = 300)
plot(g3)
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

## Finding monthly correlation

The aim is to find if there is correlation of monthly rolling volume.

``` r
Monthly_RV <- Rets %>% 
    mutate(YM = floor_date(date, "month")) %>% 
    group_by(Tickers, Region, YM) %>% 
    summarise(RV = sd(LogRet) * sqrt(252)) %>% 
    ungroup
```

    ## `summarise()` has grouped output by 'Tickers', 'Region'. You can override using
    ## the `.groups` argument.

``` r
t3 <- Monthly_RV %>%
    select(YM, Tickers, RV) %>%
    spread(Tickers, RV) %>%
    select(-YM) %>%
    cor(use = "pairwise.complete.obs")
t3
```

    ##                 J200      J433 MXWO Index SPXT Index
    ## J200       1.0000000 0.9534783  0.8634703  0.8432851
    ## J433       0.9534783 1.0000000  0.8483949  0.8288520
    ## MXWO Index 0.8634703 0.8483949  1.0000000  0.9872132
    ## SPXT Index 0.8432851 0.8288520  0.9872132  1.0000000

# Question 1B

## Setup

``` r
pacman::p_load("tidyverse", "lubridate", "tbl2xts", "rmsfuns", "PerformanceAnalytics", "RcppRoll", "glue", "RobStatTM")

SafeCore <- read_rds("data/SafeCore.rds")
Factor_Scores <- read_rds("data/Factor_Scores.rds")
Local_Index_Constituents <- read_rds("data/Index_Constituents.rds")
Idx_Daily <- read_rds("data/Index_Returns_Daily.rds")
```

Check the data I have

``` r
list(SafeCore, Idx_Daily, Local_Index_Constituents, Factor_Scores) %>% walk(glimpse)
```

    ## Rows: 9,838
    ## Columns: 5
    ## $ date    <date> 2007-01-31, 2007-01-31, 2007-01-31, 2007-01-31, 2007-01-31, 2…
    ## $ Tickers <chr> "AGL", "APA", "APB", "ARL", "BVT", "EMI", "GFI", "GND", "GRT",…
    ## $ Sector  <chr> "Resources", "Property", "Property", "Industrials", "Industria…
    ## $ Weight  <dbl> 5.901035e-02, 6.337941e-03, 3.272986e-04, 1.740618e-02, 6.7482…
    ## $ Fund    <chr> "SafeCore", "SafeCore", "SafeCore", "SafeCore", "SafeCore", "S…
    ## Rows: 16,940
    ## Columns: 4
    ## $ date    <date> 2015-01-02, 2015-01-05, 2015-01-06, 2015-01-07, 2015-01-08, 2…
    ## $ Returns <dbl> -0.004836746, -0.030113156, 0.017807820, 0.009464806, 0.012172…
    ## $ Tickers <chr> "J433", "J433", "J433", "J433", "J433", "J433", "J433", "J433"…
    ## $ Name    <chr> "Capped SWIX", "Capped SWIX", "Capped SWIX", "Capped SWIX", "C…
    ## Rows: 655,336
    ## Columns: 6
    ## $ date        <date> 2020-08-31, 2020-08-31, 2020-08-31, 2020-08-31, 2020-08-3…
    ## $ Tickers     <chr> "ABG", "AGL", "ANG", "BID", "BTI", "CFR", "CLS", "CPI", "E…
    ## $ Return      <dbl> -0.03954, -0.00401, -0.00756, -0.03182, 0.01911, 0.01991, …
    ## $ weight      <dbl> 0.011121198, 0.040941454, 0.034772782, 0.038428999, 0.0077…
    ## $ SuperSector <chr> "Financials", "Resources", "Resources", "Industrials", "In…
    ## $ Index       <chr> "LowVol", "LowVol", "LowVol", "LowVol", "LowVol", "LowVol"…
    ## Rows: 52,500
    ## Columns: 4
    ## $ date         <date> 2020-01-31, 2020-01-31, 2020-01-31, 2020-01-31, 2020-01-…
    ## $ Tickers      <chr> "ABG", "ACL", "ADH", "AEL", "AFE", "AFH", "AFT", "AFX", "…
    ## $ Factor       <chr> "Earnings Quality", "Earnings Quality", "Earnings Quality…
    ## $ Factor_Value <dbl> -0.55437448, 2.26971533, 0.06752431, -0.62692964, 0.24634…

Check how often SafeCore rebalances, and if the weights sum to 1

``` r
SafeCore %>% 
    group_by(date) %>% 
    summarise(N = n(), SumW = sum(Weight)) %>% 
    print(n = 20)
```

    ## # A tibble: 236 × 3
    ##    date           N  SumW
    ##    <date>     <int> <dbl>
    ##  1 2007-01-31    32 0.855
    ##  2 2007-02-28    32 0.852
    ##  3 2007-03-31    33 0.792
    ##  4 2007-04-30    33 0.792
    ##  5 2007-05-31    33 0.791
    ##  6 2007-06-30    35 0.796
    ##  7 2007-07-31    35 0.809
    ##  8 2007-08-31    35 0.812
    ##  9 2007-09-30    37 0.810
    ## 10 2007-10-31    37 0.809
    ## 11 2007-11-30    37 0.813
    ## 12 2007-12-31    36 0.855
    ## 13 2008-01-31    36 0.843
    ## 14 2008-02-29    36 0.828
    ## 15 2008-03-31    36 0.838
    ## 16 2008-04-30    36 0.847
    ## 17 2008-05-31    36 0.843
    ## 18 2008-06-30    34 0.831
    ## 19 2008-07-31    34 0.851
    ## 20 2008-08-31    34 0.850
    ## # ℹ 216 more rows

Creating daily stock returns

``` r
Stock_Rets <- Local_Index_Constituents %>% 
    select(date, Tickers, Return) %>% 
    distinct(date, Tickers, .keep_all = TRUE) %>% 
    mutate(Tickers = gsub(" SJ", "", Tickers))

SafeCore <- SafeCore %>% 
    mutate(Tickers = gsub(" SJ", "", Tickers))
```

cleaning up the benchmark Capped SWIX

``` r
Bench <- Idx_Daily %>% 
    filter(Tickers == "J433") %>% 
    select(date, J433 = Returns)
```

change Factor_Score to wide table

``` r
Factor_Scores <- Factor_Scores %>%
    mutate(Factor = gsub("[^A-Za-z]", "", Factor)) %>%
    select(date, Tickers, Factor, Factor_Value) %>%
    distinct(date, Tickers, Factor, .keep_all = TRUE) %>%
    spread(Factor, Factor_Value)
```

## SafeCore daily returns from holdings

I ran into an error later on where there were various stocks in SafeCore
that did not appear to have returns, to fix this, I dropped those
variables and recalculated the weights accordingly, we lost about 25% of
the weights

``` r
# unique entries into stock returns with returns values
Have_Rets <- unique(Stock_Rets$Tickers)

# Check for how many tickers have been dropped
SafeCore %>%
    group_by(date) %>%
    summarise(Weight_Dropped = sum(Weight[!Tickers %in% Have_Rets])) %>%
    summarise(Avg_Dropped = mean(Weight_Dropped), Max_Dropped = max(Weight_Dropped))
```

    ## # A tibble: 1 × 2
    ##   Avg_Dropped Max_Dropped
    ##         <dbl>       <dbl>
    ## 1      0.0787       0.245

``` r
# Reweight
SafeCore_Adj <- SafeCore %>%
    filter(Tickers %in% Have_Rets) %>%
    group_by(date) %>%
    mutate(Weight = Weight / sum(Weight)) %>% 
    ungroup()
```

combining weights at each rebalancing date with daily returns for each
stock

``` r
W_xts <- SafeCore_Adj %>%
    select(date, Tickers, Weight) %>%
    spread(Tickers, Weight) %>%
    mutate(across(-date, ~ coalesce(., 0))) %>%
    tbl_xts()

R_xts <- Stock_Rets %>%
    filter(Tickers %in% colnames(W_xts), date >= min(SafeCore$date)) %>%
    spread(Tickers, Return) %>%
    mutate(across(-date, ~ coalesce(., 0))) %>%
    tbl_xts()

Common <- intersect(colnames(W_xts), colnames(R_xts))
W_xts  <- W_xts[, Common]
R_xts  <- R_xts[, Common]

SafeCore_Port <- rmsfuns::Safe_Return.portfolio(R = R_xts, weights = W_xts, lag_weights = T, contribution = T, verbose = T, geometric = T)

SafeCore_Ret <- SafeCore_Port$returns %>% 
    xts_tbl() %>%
    rename(SafeCore = portfolio.returns)
```

Comparison of fund to benchmark side by side

``` r
Port_vs_Bench <- SafeCore_Ret %>% 
    inner_join(Bench, by = "date") %>% 
    filter(!is.na(J433))
```

## Does SafeCore handle volatility well?

To answer this we look at four measures: returns, volatility and Sharpe
ratio, is it more volatile than the benchmark? semi-deviation, VaR, ES
and maximum drawdown, How much does it lose when things go badly? look
at the high volatility periods found in 1A and look how SafeCore
performed in them. And looking if SafeCore has a beta abovre one in
highly volatile points.

``` r
PvB_xts <- Port_vs_Bench %>% 
    tbl_xts()

t4 <- PerformanceAnalytics::table.AnnualizedReturns(PvB_xts, scale = 252)
t4
```

    ##                           SafeCore   J433
    ## Annualized Return           0.1727 0.1590
    ## Annualized Std Dev          0.1692 0.1639
    ## Annualized Sharpe (Rf=0%)   1.0207 0.9697

``` r
t5 <- PerformanceAnalytics::table.DownsideRisk(PvB_xts)
t5
```

    ##                               SafeCore    J433
    ## Semi Deviation                  0.0076  0.0074
    ## Gain Deviation                  0.0070  0.0069
    ## Loss Deviation                  0.0072  0.0071
    ## Downside Deviation (MAR=210%)   0.0124  0.0122
    ## Downside Deviation (Rf=0%)      0.0072  0.0070
    ## Downside Deviation (0%)         0.0072  0.0070
    ## Maximum Drawdown                0.1815  0.1414
    ## Historical VaR (95%)           -0.0167 -0.0159
    ## Historical ES (95%)            -0.0232 -0.0225
    ## Modified VaR (95%)             -0.0168 -0.0162
    ## Modified ES (95%)              -0.0245 -0.0243

``` r
t6 <- PerformanceAnalytics::table.SFM(PvB_xts[, "SafeCore"], PvB_xts[, "J433"], scale = 252)
```

    ## Registered S3 method overwritten by 'robustbase':
    ##   method          from     
    ##   hatvalues.lmrob RobStatTM

``` r
t6
```

    ##                     SafeCore to J433
    ## Alpha                         0.0000
    ## Beta                          1.0036
    ## Alpha Robust                  0.0000
    ## Beta Robust                   1.0118
    ## Beta+                         0.9988
    ## Beta-                         0.9975
    ## Beta+ Robust                  1.0119
    ## Beta- Robust                  1.0053
    ## R-squared                     0.9461
    ## R-squared Robust              0.9217
    ## Annualized Alpha              0.0121
    ## Correlation                   0.9727
    ## Correlation p-value           0.0000
    ## Tracking Error                0.0393
    ## Active Premium                0.0137
    ## Information Ratio             0.3481
    ## Treynor Ratio                 0.1720

``` r
g4 <- Port_vs_Bench %>%
    gather(Series, Ret, -date) %>%
    group_by(Series) %>%
    arrange(date) %>%
    mutate(RV_1y = RcppRoll::roll_sd(log(1 + Ret), 252, fill = NA, align = "right") * sqrt(252)) %>%
    ungroup() %>%
    filter(!is.na(RV_1y)) %>%
    ggplot() +
    geom_line(aes(date, RV_1y, colour = Series), size = 1) +
    fmxdat::theme_fmx() +
    labs(x = "", y = "Annualised volatility",
         title = "Rolling 1-year volatility: SafeCore vs Capped SWIX")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
g4_final <- fmxdat::finplot(g4, y.pct = T)
ggsave("Figures/Q1B_FIg1_AnnVol.pdf", plot = g4_final, width = 8, height = 5, dpi = 300)
plot(g4_final)
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
pdf("Figures/Q1B_Fig2__Drawdowns.pdf", width = 8, height = 5)   # size in inches
PerformanceAnalytics::chart.Drawdown(PvB_xts, legend.loc = "bottomleft",
                                     main = "Drawdowns: SafeCore vs Capped SWIX")

pdf("Figures/Q1B_Fig3_RollingBeta.pdf", width = 8, height = 5)
PerformanceAnalytics::chart.RollingRegression(PvB_xts[, "SafeCore"], PvB_xts[, "J433"],
                                              width = 252, attribute = "Beta",
                                              main = "Rolling 1-year beta to Capped SWIX")

# in order to make sure that it creates all the pdf graphs correctly
dev.list()       
```

    ## png pdf pdf 
    ##   2   3   4

``` r
graphics.off()   
dev.list() 
```

    ## pdf 
    ##   2

## Main source of volatility

Here i try to determine what the stocks contribute to the portfolio
volatility using the past year returns at the latest rebalance.

``` r
Last_Date <- max(SafeCore$date)

W_last <- SafeCore_Adj %>% filter(date == Last_Date, Weight > 0)

R_window <- Stock_Rets %>%
    filter(Tickers %in% W_last$Tickers,
           date >  Last_Date - years(1), date <= Last_Date) %>%
    spread(Tickers, Return) %>%
    mutate(across(-date, ~ coalesce(., 0))) %>%
    tbl_xts()

w_vec <- W_last$Weight[match(colnames(R_window), W_last$Tickers)]

Risk_Contrib <- PerformanceAnalytics::StdDev(R_window, weights = w_vec,
                                             portfolio_method = "component")

Risk_Tbl <- tibble(Tickers     = colnames(R_window),
                   Weight      = w_vec,
                   Pct_Risk    = as.numeric(Risk_Contrib$pct_contrib_StdDev)) %>%
    mutate(Risk_minus_Weight = Pct_Risk - Weight) %>%
    arrange(desc(Pct_Risk))
Risk_Tbl # t7
```

    ## # A tibble: 38 × 4
    ##    Tickers  Weight Pct_Risk Risk_minus_Weight
    ##    <chr>     <dbl>    <dbl>             <dbl>
    ##  1 ANG     0.120     0.278             0.158 
    ##  2 GFI     0.0748    0.172             0.0976
    ##  3 VAL     0.0442    0.104             0.0601
    ##  4 MTN     0.0880    0.0631           -0.0249
    ##  5 SBK     0.101     0.0618           -0.0396
    ##  6 PAN     0.0275    0.0573            0.0298
    ##  7 ABG     0.0723    0.0412           -0.0311
    ##  8 BHG     0.0469    0.0353           -0.0116
    ##  9 FSR     0.0515    0.0314           -0.0200
    ## 10 SSW     0.00988   0.0244            0.0145
    ## # ℹ 28 more rows

``` r
g5 <- Risk_Tbl %>%
    slice_max(Pct_Risk, n = 10) %>%
    gather(Measure, Value, Weight, Pct_Risk) %>%
    ggplot() +
    geom_col(aes(reorder(Tickers, -Value), Value, fill = Measure), position = "dodge") +
    fmxdat::theme_fmx() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "", y = "", title = "SafeCore: share of risk vs share of capital (top 10)")
g5
```

![](README_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
ggsave("Figures/Q1B_FIg4_RiskShare.pdf", plot = g5, width = 8, height = 5, dpi = 300)
plot(g4_final)
```

![](README_files/figure-markdown_github/unnamed-chunk-23-2.png)

``` r
Sectors <- Local_Index_Constituents %>%
    mutate(Sector = SuperSector) %>% 
    mutate(Tickers = gsub(" SJ", "", Tickers)) %>%
    distinct(Tickers, Sector) 

t8 <- Risk_Tbl %>%
    left_join(Sectors, by = "Tickers") %>%
    group_by(Sector) %>%
    summarise(Weight = sum(Weight), Pct_Risk = sum(Pct_Risk)) %>%
    arrange(desc(Pct_Risk))
t8
```

    ## # A tibble: 3 × 3
    ##   Sector      Weight Pct_Risk
    ##   <chr>        <dbl>    <dbl>
    ## 1 Resources    0.401    0.682
    ## 2 Financials   0.339    0.186
    ## 3 Industrials  0.285    0.141

## Comparing momentum and low volatility scores to third party

For this analysis I chose to use momentum which is cumulative returns
over the past 6 months, because very recent winners tend to give some of
it back, do the latest month adds noise, low volatility score which is
minus the 6-month realised volatility, the minus sign makes a higher
score mean less volatile, which matches the third party’s convention
that a higher number means more of the factor, and finally the z-score
across stocks on each date, capped at about 3, which puts the score on
the same scale as the third party score and limits the influence of
extreme stocks.

``` r
Monthly_Rets <- Stock_Rets %>%
    mutate(YM = floor_date(date, "month")) %>%
    group_by(Tickers, YM) %>%
    summarise(Ret_M = prod(1 + coalesce(Return, 0)) - 1) %>%
    ungroup()
```

    ## `summarise()` has grouped output by 'Tickers'. You can override using the
    ## `.groups` argument.

``` r
Mom <- Monthly_Rets %>%
    group_by(Tickers) %>%
    arrange(YM) %>%
    mutate(Momentum_Own = RcppRoll::roll_prod(1 + lag(Ret_M), 5, fill = NA, align = "right") - 1) %>%
    ungroup() %>%
    select(Tickers, YM, Momentum_Own)

LowVol <- Stock_Rets %>%
    group_by(Tickers) %>%
    arrange(date) %>%
    mutate(Vol_6m = RcppRoll::roll_sd(log(1 + Return), 126, fill = NA, align = "right") * sqrt(252)) %>%
    mutate(YM = floor_date(date, "month")) %>%
    group_by(Tickers, YM) %>%
    filter(date == last(date)) %>% 
    ungroup() %>%
    transmute(Tickers, YM, LowVol_Own = -Vol_6m)

Own_Scores <- Mom %>%
    inner_join(LowVol, by = c("Tickers", "YM")) %>%
    group_by(YM) %>%
    mutate(across(c(Momentum_Own, LowVol_Own),
                  ~ pmin(pmax((. - mean(., na.rm = T)) / sd(., na.rm = T), -3), 3))) %>%
    ungroup()

TP_Scores <- Factor_Scores %>%
    mutate(Tickers = gsub(" SJ", "", Tickers),
           YM = floor_date(as.Date(date), "month")) %>%
    select(Tickers, YM, Momentum_TP = Momentum, LowVol_TP = VolatilitySensitivity) 

Compare <- Own_Scores %>% inner_join(TP_Scores, by = c("Tickers", "YM"))

Score_Cor <- Compare %>%
    group_by(YM) %>%
    summarise(
        N_Mom    = sum(complete.cases(Momentum_Own, Momentum_TP)),
        N_LowVol = sum(complete.cases(LowVol_Own,   LowVol_TP)),
        Momentum = ifelse(N_Mom >= 10,
                          cor(Momentum_Own, Momentum_TP, method = "spearman", use = "complete.obs"),
                          NA_real_),
        LowVol   = ifelse(N_LowVol >= 10,
                          cor(LowVol_Own, LowVol_TP, method = "spearman", use = "complete.obs"),
                          NA_real_)) %>%
    ungroup()

g6 <- Score_Cor %>%
    select(YM, Momentum, LowVol) %>% 
    gather(Factor, Rank_Cor, Momentum, LowVol) %>%
    filter(!is.na(Rank_Cor)) %>%
    ggplot() +
    geom_line(aes(YM, Rank_Cor, colour = Factor), size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    fmxdat::theme_fmx() +
    labs(x = "", y = "Spearman rank correlation",
         title = "Own vs third-party factor scores: how much do they agree?")

plot(g6)
```

![](README_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
ggsave("Figures/Q1B_FIg5_SpearRank.pdf", plot = g6, width = 8, height = 5, dpi = 300)

g7 <- Compare %>%
    filter(YM == max(YM)) %>%
    ggplot() +
    geom_point(aes(LowVol_TP, LowVol_Own), alpha = 0.6, colour = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    fmxdat::theme_fmx() +
    labs(x = "Third-party Low-Vol score", y = "Own Low-Vol score",
         title = "Low-Vol scores at the latest date")
plot(g7)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-markdown_github/unnamed-chunk-24-2.png)

``` r
ggsave("Figures/Q1B_FIg5_LowVol.pdf", plot = g7, width = 8, height = 5, dpi = 300)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

## SafeCore’s factor exposures vs J433, and composite strategy scores

Here I look at portfolio exposure which is a weighted average of the
stock score for both SafeCore and J433, active exposure where a positive
value means SafeCore tilts towards the factor and a negative score tilts
away from it, and composite strategies which show the investment style
that SafeCore resembles.

``` r
J433_W <- Local_Index_Constituents %>%
    filter(Index == "Capped SWIX") %>%
    mutate(Weight = weight) %>% 
    mutate(Tickers = gsub(" SJ", "", Tickers), YM = floor_date(date, "month")) %>%
    group_by(Tickers, YM) %>% filter(date == last(date)) %>% ungroup() %>%
    select(Tickers, YM, Weight)

Fund_W <- SafeCore_Adj %>%
    mutate(YM = floor_date(date, "month")) %>%
    select(Tickers, YM, Weight)

All_Scores <- Factor_Scores %>%
    mutate(Tickers = gsub(" SJ", "", Tickers), YM = floor_date(as.Date(date), "month")) %>%
    select(-date) %>%
    left_join(Own_Scores, by = c("Tickers", "YM"))

All_Scores <- All_Scores %>%
    mutate(Quality = EarningsQuality) %>% 
    mutate(LowVol = VolatilitySensitivity) %>% 
    mutate(Defensive   = (LowVol + Quality) / 2,
           Quality_Mom = (Quality + Momentum) / 2,
           Value_Comp  = Value)

Factor_Cols <- c("Value", "Momentum", "Quality", "LowVol", "Growth",
                 "Momentum_Own", "LowVol_Own", "Defensive", "Quality_Mom")

Exposure <- function(W) {
    W %>%
        inner_join(All_Scores, by = c("Tickers", "YM")) %>%
        group_by(YM) %>%
        summarise(across(all_of(Factor_Cols), ~ sum(Weight * ., na.rm = T) / sum(Weight[!is.na(.)]))) %>%
        ungroup()
}

Exp_Tbl <- bind_rows(Exposure(Fund_W) %>% mutate(Portfolio = "SafeCore"),
                     Exposure(J433_W) %>% mutate(Portfolio = "J433")) %>%
    filter(YM %in% unique(Fund_W$YM)) 

Active <- Exp_Tbl %>%
    gather(Factor, Score, all_of(Factor_Cols)) %>%
    spread(Portfolio, Score) %>%
    mutate(Active = SafeCore - J433)

g8 <- Active %>%
    filter(Factor %in% c("Value", "Momentum", "Quality", "LowVol", "Growth")) %>%
    ggplot() +
    geom_line(aes(YM, Active, colour = Factor), size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    fmxdat::theme_fmx() +
    labs(x = "", y = "Active exposure (z-score)",
         title = "SafeCore factor tilts relative to the Capped SWIX")
plot(g8)
```

    ## Warning: Removed 35 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](README_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
ggsave("Figures/Q1B_FIg6_ActiveExp.pdf", plot = g8, width = 8, height = 5, dpi = 300)
```

    ## Warning: Removed 35 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

``` r
g9 <- Active %>%
    filter(YM == max(YM)) %>%
    ggplot() +
    geom_col(aes(reorder(Factor, Active), Active, fill = Active > 0)) +
    coord_flip() +
    fmxdat::theme_fmx() +
    guides(fill = "none") +
    labs(x = "", y = "Active exposure (z-score)",
         title = "SafeCore vs J433: latest factor and composite exposures")

plot(g9)
```

![](README_files/figure-markdown_github/unnamed-chunk-25-2.png)

``` r
ggsave("Figures/Q1B_FIg7_ActiveExpSvJ.pdf", plot = g9, width = 8, height = 5, dpi = 300)
```

# Question 1C

## Setup

``` r
pacman::p_load("tidyverse", "lubridate", "RcppRoll", "fmxdat", "glue")

Local_Index_Constituents <- read_rds("data/Index_Constituents.rds")
Uncertainty_Indexes <- read_rds("data/Econ_Uncertainty_Indexes.rds")
Gl_Cncy_Vol_Index <- read_rds("data/Global_Currency_Vol_Index.rds")
Commods <- read_rds("data/Commodities.rds")

Start_Analysis <- as.Date("2020-01-01")
Start_Data <- as.Date("2019-07-01")
```

## Daily stock returns and Capped SWIX weights

Here I found the daily returns and Capped SWIX rates which I used to sum
to over 1 the stocks that have a return that day.

``` r
J433 <- Local_Index_Constituents %>%
    filter(Index == "Capped SWIX") %>% 
    mutate(Weight = weight) %>% 
    mutate(Tickers = gsub(" SJ", "", Tickers)) %>%
    select(date, Tickers, Return, Weight) %>%
    filter(date >= Start_Data) %>%
    arrange(date)

J433 <- J433 %>%
    filter(!is.na(Return), !is.na(Weight)) %>%
    group_by(date) %>%
    mutate(W = Weight / sum(Weight)) %>%
    ungroup()
```

## CSV, weighted and unweighted

the CSV formular for weighted is
*s**q**r**t*(*s**u**m**w*<sub>*i*</sub>(*R*<sub>*i*</sub> − *(̄**R*))<sup>2</sup>).
unweighted is simply the standard deviation of stock returns across
stocks.

``` r
J433 <- J433 %>%
    mutate(Return = ifelse(Return >  0.25,  0.25,
                           ifelse(Return < -0.25, -0.25, Return)))

CSV <- J433 %>%
    group_by(date) %>%
    summarise(Rbar_W = sum(W * Return),
              CSV_W = sqrt(sum(W * (Return - Rbar_W)^2)),
              CSV_Unwtd = sd(Return),
              N = n()) %>%
    ungroup() %>%
    mutate(CSV_W_3m = RcppRoll::roll_mean(CSV_W,     63, fill = NA, align = "right"),
           CSV_Unwtd_3m = RcppRoll::roll_mean(CSV_Unwtd, 63, fill = NA, align = "right"))

g10 <- CSV %>%
    filter(date >= Start_Analysis) %>%
    select(date, Weighted = CSV_W_3m, Unweighted = CSV_Unwtd_3m) %>%
    gather(Measure, CSV, -date) %>%
    ggplot() +
    geom_line(aes(date, CSV, colour = Measure), linewidth = 1) +
    fmxdat::theme_fmx() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(x = "", y = "Daily cross-sectional volatility (3m avg)",
         title = "Capped SWIX dispersion since 2020: weighted vs unweighted",
         caption = "Rolling 63-day average of daily CSV across J433 constituents")
g10
```

    ## Warning: Removed 124 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](README_files/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
ggsave("Figures/Q1C_FIg1_RollingCSV.pdf", plot = g10, width = 8, height = 5, dpi = 300)
```

    ## Warning: Removed 124 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

## Rolling average pairwise correlation

At the end of each month I take the 20 largest Capped SWIX stocks and
compute the correlation matrix of their daily returns over the past 63
trading days, and average the pairwise correlations. High = stocks move
together (one market factor dominates, little room for stock picking)

``` r
Rets_Wide <- J433 %>%
    select(date, Tickers, Return) %>%
    spread(Tickers, Return)

Trading_Days <- Rets_Wide$date

Month_Ends <- tibble(date = Trading_Days) %>%
    mutate(YM = floor_date(date, "month")) %>%
    group_by(YM) %>% filter(date == last(date)) %>% ungroup() %>%
    filter(date >= Start_Analysis, date >= Trading_Days[63]) %>%
    pull(date)

Avg_Pair_Cor <- function(d, N_Top = 20, Window = 63) {
    Last_W_Date <- max(J433$date[J433$date <= d])
    Top <- J433 %>% 
        filter(date == Last_W_Date) %>% 
        slice_max(W, n = N_Top) %>% 
        pull(Tickers)

    Win_Dates <- tail(Trading_Days[Trading_Days <= d], Window)
    M <- Rets_Wide %>% filter(date %in% Win_Dates) %>% select(any_of(Top))
    M <- M[, colSums(!is.na(M)) >= 0.8 * Window, drop = FALSE]

    if (ncol(M) < 2) return(NA_real_) 
    C <- cor(M, use = "pairwise.complete.obs")
    mean(C[upper.tri(C)], na.rm = TRUE)
}

Roll_Cor <- tibble(date = Month_Ends) %>%
    mutate(Avg_Cor = map_dbl(date, Avg_Pair_Cor))

g11 <- Roll_Cor %>%
    ggplot() +
    geom_line(aes(date, Avg_Cor), colour = "darkred", linrwidth = 1) +
    fmxdat::theme_fmx() +
    labs(x = "", y = "Average pairwise correlation",
         title = "Rolling 3-month average correlation among the top 20 J433 stocks")
```

    ## Warning in geom_line(aes(date, Avg_Cor), colour = "darkred", linrwidth = 1):
    ## Ignoring unknown parameters: `linrwidth`

``` r
g11
```

![](README_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
ggsave("Figures/Q1C_FIg2_PairCorr.pdf", plot = g11, width = 8, height = 5, dpi = 300)
```

## Alpha opportunity panel

It stacks three monthly series: weighted CSV, average correlation and
market volatility. the best environment is high CSV and low correlation,
the crisis pattern is both CSV and correlation high.

``` r
Monthly_Opp <- CSV %>%
    arrange(date) %>%
    mutate(Mkt_Vol = RcppRoll::roll_sd(Rbar_W, 63, fill = NA, align = "right") * sqrt(252)) %>%
    filter(date %in% Month_Ends) %>%
    select(date, CSV_W_3m, CSV_Unwtd_3m, Mkt_Vol) %>%
    left_join(Roll_Cor, by = "date")

g12 <- Monthly_Opp %>%
    select(date, `Weighted CSV (3m avg)` = CSV_W_3m,
           `Avg pairwise correlation` = Avg_Cor,
           `Market volatility (ann.)` = Mkt_Vol) %>%
    gather(Measure, Value, -date) %>%
    ggplot() +
    geom_line(aes(date, Value), colour = "steelblue", size = 1) +
    facet_wrap(~Measure, ncol = 1, scales = "free_y") +
    fmxdat::theme_fmx() +
    labs(x = "", y = "", title = "Alpha opportunity in the local market since 2020",
         subtitle = "Opportunity is highest when dispersion is high AND correlation is low")
g12
```

![](README_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
ggsave("Figures/Q1C_FIg3_Alpha.pdf", plot = g12, width = 8, height = 5, dpi = 300)
```

## How does alpha opportunity move with global macro factors

In order to check this, I used global economic policy uncertainty, oil
volatility and global fx volatility. what i got from this is a
correlation table which tells me if dispersion rises or falls with the
factors, and stratification which sorts the months into low, medium and
high terciles of each macro factor, then compares the average dispersion
and correlation in each.

``` r
EPU <- Uncertainty_Indexes %>%
    filter(Name == "EPUCGLCP") %>%
    mutate(YM = floor_date(as.Date(date), "month")) %>%
    group_by(YM) %>% summarise(EPU = mean(TRI, na.rm = T)) %>% ungroup()

Oil_Vol <- Commods %>%
    filter(Name == "Oil_Brent") %>% 
    arrange(date) %>%
    mutate(r = log(TRI) - log(lag(TRI)), 
           YM = floor_date(as.Date(date), "month")) %>%
    group_by(YM) %>% 
    summarise(Oil_Vol = sd(r, na.rm = T) * sqrt(252)) %>% 
    ungroup()

FX_Vol <- Gl_Cncy_Vol_Index %>%
    mutate(YM = floor_date(as.Date(date), "month")) %>%
    group_by(YM) %>% 
    summarise(Global_FX_Vol = mean(TRI, na.rm = T)) %>% 
    ungroup()

Macro <- Monthly_Opp %>%
    mutate(YM = floor_date(date, "month")) %>%
    left_join(EPU,     by = "YM") %>%
    left_join(Oil_Vol, by = "YM") %>%
    left_join(FX_Vol,  by = "YM") %>%
    na.omit()

t9 <- Macro %>%
    select(CSV_W_3m, Avg_Cor, EPU, Oil_Vol, Global_FX_Vol) %>%
    cor() %>%
    round(2) %>%
    .[c("CSV_W_3m", "Avg_Cor"), c("EPU", "Oil_Vol", "Global_FX_Vol")]
t9
```

    ##           EPU Oil_Vol Global_FX_Vol
    ## CSV_W_3m 0.28    0.48          0.04
    ## Avg_Cor  0.08    0.43          0.46

``` r
Strat <- Macro %>%
    select(date, CSV_W_3m, Avg_Cor, EPU, Oil_Vol, Global_FX_Vol) %>%
    gather(Macro_Factor, Macro_Value, EPU, Oil_Vol, Global_FX_Vol) %>%
    group_by(Macro_Factor) %>%
    mutate(Tercile = ntile(Macro_Value, 3)) %>%
    group_by(Macro_Factor, Tercile) %>%
    summarise(Avg_CSV = mean(CSV_W_3m),
              Avg_Cor = mean(Avg_Cor),
              N_Months = n()) %>%
    ungroup()
```

    ## `summarise()` has grouped output by 'Macro_Factor'. You can override using the
    ## `.groups` argument.

``` r
Strat
```

    ## # A tibble: 9 × 5
    ##   Macro_Factor  Tercile Avg_CSV Avg_Cor N_Months
    ##   <chr>           <int>   <dbl>   <dbl>    <int>
    ## 1 EPU                 1  0.0178   0.204       24
    ## 2 EPU                 2  0.0189   0.237       24
    ## 3 EPU                 3  0.0193   0.198       23
    ## 4 Global_FX_Vol       1  0.0187   0.198       24
    ## 5 Global_FX_Vol       2  0.0187   0.195       24
    ## 6 Global_FX_Vol       3  0.0187   0.248       23
    ## 7 Oil_Vol             1  0.0177   0.173       24
    ## 8 Oil_Vol             2  0.0181   0.215       24
    ## 9 Oil_Vol             3  0.0204   0.253       23

``` r
g13 <- Strat %>%
    select(Macro_Factor, Tercile, `Weighted CSV` = Avg_CSV, `Avg correlation` = Avg_Cor) %>%
    gather(Measure, Value, `Weighted CSV`, `Avg correlation`) %>%
    ggplot() +
    geom_col(aes(factor(Tercile), Value, fill = Macro_Factor), position = "dodge") +
    facet_wrap(~Measure, scales = "free_y") +
    fmxdat::theme_fmx() +
    labs(x = "Macro factor tercile (1 = low, 3 = high)", y = "",
         title = "Dispersion and correlation by global macro regime (since 2020)")
g13
```

![](README_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
ggsave("Figures/Q1C_FIg4_DispCorr.pdf", plot = g13, width = 8, height = 5, dpi = 300)
```

### Priinting all the tables for question 1

``` r
dir.create("Tables", showWarnings = FALSE)

saveRDS(tibble::lst(t1, t2, t3), "Tables/Q1A_Tables.rds")

saveRDS(tibble::lst(t4, t5, t6, Risk_Tbl, t8), "Tables/Q1B_Tables.rds")

saveRDS(tibble::lst(t9, Strat), "Tables/Q1C_Tables.rds")
```
