#  hull.R - hull moving average trading system set for single run
rm(list=ls())         ##############
library(conflicted)
library(tidyverse, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(tidyquant, quietly = TRUE)
library(Rcpp, quietly = TRUE)
library(TTR, quietly = TRUE)
library(plotly, quietly = TRUE)
library(profvis, quietly = TRUE)
library(rlang, quietly = TRUE)
library(clock, quietly = TRUE)
library(ggrepel, quietly = TRUE)
library(here)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

start_time <- Sys.time()               # for internal monitoring
run_time <- paste0(" ", get_hour(start_time), "-", get_minute(start_time))

# calculates pnl summary statistics
split_fun <- function(data, column_name, factor_name) {
  summary_stats <- data |>
    mutate(category = ifelse({{ column_name }} >= 0, "Positive", "Negative")) |>
    group_by({{ factor_name }}, category) |>
    summarise(
      sum = sum({{ column_name }}),
      count = n(),
      mean = mean({{ column_name }}),
      median = median({{ column_name }}),
      sd = sd({{ column_name }}),
      min = min({{ column_name }}),
      max = max({{ column_name }})
    ) |>
    select({{ factor_name }}, category, sum, count, everything())
  return(summary_stats)
}
## C code for EMA calculation with no leading NA  ###############
sourceCpp(
  code =
    "
     #include <Rcpp.h>
     // [[Rcpp::export]]
     Rcpp::NumericVector ewmaRcpp(Rcpp::NumericVector x, double a){
       int n = x.length();
       Rcpp::NumericVector s(n);
       s[0] = x[0];
       if (n > 1) {
         for (int i = 1; i < n; i++) {
           s[i] =  s[i-1] + (x[i] - s[i-1])/(( a + 1)/2);
         }
       }
       return s;
     }
    ")    ##################

product <- "ES"  #####  <- Yes, name it here. ES or NQ or whatever
fast_seq <- 50
slow_seq <- 60
runs <- expand.grid(slow=seq(slow_seq, slow_seq, 1), fast=seq(fast_seq, fast_seq, 1))

df_og <- read_csv("hull_one_minute.csv", col_names = TRUE)

# discern time interval from input file
df <- df_og
first_row_time <- df$time[1] ; second_row_time <- df$time[2] ; third_row_time <- df$time[3]
interval <- min(as.numeric(difftime(second_row_time, first_row_time, units = "secs")),
                as.numeric(difftime(third_row_time, second_row_time, units = "secs")))
if(interval <= 0) Warning("HOLY SHIT! WE HAVE REACHED THE END OF TIME!")
candles <- if(interval>=3600) {
  sprintf("%.0f hr", interval/3600) 
} else if (interval>=60) {
  sprintf("%.0f min", interval/60)
} else {
  sprintf("%.0f sec", interval)
}
start_date <- min(df$time) 
end_date <- max(df$time)
date_range <- as.numeric(difftime(end_date, start_date, units = "days"))
dates_run <- floor_date(start_date, unit="days"):floor_date(end_date, unit = "days")
witching_hour <- date_time_build(get_year(start_date), get_month(start_date),
                                 get_day(start_date), 21L, 0L, 0L, zone="UTC")
last_call <- date_seq(add_seconds(witching_hour, -interval), to=end_date, by=86400)

the_high <- max(df$high) ; the_low <- min(df$low) ; the_range <- the_high - the_low
trades_global = tibble() 
results <- tibble() # create the file to collect the results of each run
epoch <- paste0(get_month(start_date),"-", get_day(start_date), "-",
                get_year(start_date)-2000," to ", get_month(end_date), 
                "-", get_day(end_date), "-", get_year(end_date)-2000)


start_value <- 1162  # required overnight margin in points
skid <- 0   # skid is expected loss on trade execution, set to ZERO for testing!


######################## optimization sequence ########################
# for (j in seq_len(nrow(runs))) {
j <- 1

df <- df_og
fast_lag <- runs$fast[j]
slow_lag <- runs$slow[j]
if(fast_lag == slow_lag) {
  results[j,1:17] <- as_tibble_row(
    c(j=j, fast_lag=fast_lag, slow_lag=slow_lag, ICAGR=0, drawdown=0,
      bliss=0, lake=0, end_value=0, trade_test=0,
      trade_count=0, wins=0, losses=0, win_rate=0,
      trade_total_pnl=0, won=0, lost=0, dollar_won=0),
    .name_repair = "universal")
  next
}

# exponential moving averages
df$Efast <- ewmaRcpp(df$close, fast_lag)
df$Eslow <- ewmaRcpp(df$close, slow_lag)

df <- df |>
  select(time:close, Volume, Efast, Eslow) |>
  mutate(time = as.POSIXct(time, tz = "UTC"), # Ensure time is in POSIXct format
         h_yc = high - lag(close),
         yc_l = lag(close) - low,
         range = high - low, 
         ATR = pmax(range, h_yc, yc_l, na.rm = TRUE),
         fast = HMA(close, fast_lag),
         slow = (HMA(close, slow_lag) +1e-6), 
         dfast = if_else(fast-lag(fast)==0, 1e-7, fast-lag(fast)),
         dslow = if_else(slow-lag(slow)==0, 1e-7, slow-lag(slow)),
         cross = dfast,
         # cross = fast - Eslow,
         # cross = dslow,    #  cross = fast - slow,
         on = if_else(cross > 0 & lag(cross) < 0, 1, 0), 
         off = if_else(cross < 0 & lag(cross) > 0, -1, 0),
         signal = on + off) |>
  drop_na(on) |>
  drop_na(off)

df$off[1] <- 0

# close and restart trades on market schedule 23/5
closing_time <- which(df$time %in% last_call) |>
  append(nrow(df))
closing_time <- unique(closing_time)
restart_time <- (which(df$time %in% last_call) +1 ) |>
  append(1, after=0)
restart_time <- restart_time[!restart_time %in% (nrow(df)+1) ]

# reopen trades with market
for (num in restart_time) {  
  df$on[num] <-  if_else(df$cross[num] > 0, 1, 0)  # catch first row signal, if there
  df$signal[num] <- df$on[num]
}

df <- df |>
  mutate(open_trade = cumsum(signal),
         buy_date = if_else(on == 1, as_datetime(lead(time), tz="America/New_York"), NA),
         sell_date = if_else(off == -1, as_datetime(lead(time), tz="America/New_York"), NA),
         buy_price = if_else(on == 1, lead(open) + skid, NA),
         sell_price = if_else(off == -1, lead(open) - skid, 0),
         buy_amount = 1,) |>  # no work on trade sizing yet, important though it is
  fill(buy_price) |>
  fill(buy_date)  

# close trades with market
for(k in closing_time) {
  if(df$on[k] == 1) {     # no new trades in last period
    df$on[k] = 0 ; df$signal[k] = 0
  } else if (df$cross[k] >0 | df$signal[k] == -1) { 
    df$off[k] <- -1
    df$signal[k] <- -1
    df$open_trade[k] <- 0
    df$sell_date[k] <- df$time[k]
    df$sell_price[k] <- df$close[k] - skid       
  }
}

# cume trade metrics
df <- df |>
  drop_na(buy_price) |>
  mutate(
    trade_pnl = if_else(signal == -1, (sell_price - buy_price) * buy_amount, 0),
    win_lose = as.factor(if_else(trade_pnl<0, "loser", "winner")),
    closed_pnl = cumsum(trade_pnl) + start_value,
    open_pnl = (close - buy_price) * buy_amount * open_trade,
    equity = open_pnl + closed_pnl,
    highwater = cummax(equity),
    lake = highwater - equity,
    drawdown = lake / highwater)

# summary trade table 
trade_test <- sum(df$signal) 
if(trade_test != 0) warning("HOLY SHIT! THE TRADES ARE FUCKED!")
trades <- df |>
  select(off, buy_date:drawdown) |>
  filter(off == -1) |>
  mutate(
    off=NULL, open_trade=NULL, buy_amount=NULL)

trades <- trades |>   #  MAE calc, make MFE too #######################
mutate(MAE = map_dbl(1:n(), ~ {
  row <- .x
  df |>
    filter(time >= trades$buy_date[row], time <= trades$sell_date[row]) |>
    pull(low) |>
    min(na.rm = TRUE)
}), .after=win_lose,
MAE_mark = MAE - buy_price,
MAE_percent = (MAE / buy_price) - 1,
MFE = map_dbl(1:n(), ~ {
  row <- .x
  df |>
    filter(time >= trades$buy_date[row], time <= trades$sell_date[row]) |>
    pull(high) |>
    max(na.rm = TRUE)
}),
MFE_mark = MFE - buy_price,
MFE_percent = (MFE / buy_price) - 1,
trade_pnl_percent = trade_pnl / buy_price)

# risk and return calculation
end_value <- df$equity[nrow(df)]  
ratio <- end_value/ start_value
ICAGR <- if_else(ratio <= 0, 0, log(ratio)/(date_range/365.25))
drawdown <- max(df$drawdown)
lake <- sum(df$lake) / sum(df$equity)
bliss <- ICAGR / drawdown
trade_count <- nrow(trades)
trade_total_pnl <- sum(trades$trade_pnl)
zz <- split_fun(trades, trade_pnl)
wins <- zz[[2,3]] ; losses <- zz[[1,3]] ; won <- zz[[2,2]]; lost <- zz[[1,2]]
win_rate <- wins/trade_count ; dollar_won <- -zz[[2,4]]/zz[[1,4]]
results[j,1:17] <- as_tibble_row(          
  c(j=j, fast_lag=fast_lag, slow_lag=slow_lag, ICAGR=ICAGR, drawdown=drawdown, 
    bliss=bliss, lake=lake, end_value=end_value, trade_test=trade_test, 
    trade_count=trade_count, wins=wins, losses=losses, win_rate=win_rate,
    trade_total_pnl=trade_total_pnl, won=won, lost=lost, dollar_won=dollar_won),
  .name_repair = "universal")

# accumulate the trades into the global trade table
trade_tmp <- trades |>
  mutate(MA_slow = slow_lag,
         MA_fast = fast_lag)
trades_global <- trades_global |>
  bind_rows(trade_tmp)

# }       #################### optimization loop end    ##########################

# save the results and trades_global files
run_id <- paste0( " ", candles," fast ", min(runs$fast), "-", max(runs$fast), 
                  " slow ",min(runs$slow), "-", max(runs$slow), " fr ", epoch)
results_file_name <- paste0(here("output", "results "), nrow(results), " runs ", 
                            run_id, run_time, ".csv", sep="")
write_csv(results, results_file_name)
trade_file_name <- paste0(here("output", "trades "), nrow(runs), " runs", run_id, run_time, ".csv", sep="")
write_csv(trades_global, trade_file_name)

forever <- Sys.time() - start_time
secs <- forever  / nrow(runs)
sprintf("Yo, %1.2f min,  %1.2f per run, %i runs, %s records, over %1.2f days of data", 
        forever, secs, nrow(results), format(nrow(df), big.mark=","), date_range)


##########################

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "lm", linewidth = 10) +
  geom_line(aes(y=fast, alpha = 0.2)) +
  geom_line(aes(y=slow, alpha = 0.2)) +
  geom_line(aes(y=Efast, alpha = 0.2)) +
  geom_line(aes(y=Eslow, alpha = 0.2)) +
  labs(title=paste("Run completed!"),
       subtitle=paste0(candles, " periods, ", round(date_range, 0),
                       "D of data, ", epoch,",   fast: ", min(runs$fast), "-", 
                       max(runs$fast), " slow: ",min(runs$slow), "-", 
                       max(runs$slow),", ",  "    High: ", the_high, " Low: ",
                       the_low)) +
  theme(legend.position = "none")

  