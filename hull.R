#  long-short exponential moving average  trading system
rm(list=ls())
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
library(here)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)
theme_set(theme_light())                # ggplot theme or _bw()


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
################ C code for EMA calculation with no leading NA
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
    ")


# start_time <- Sys.time()

# files <- tibble(dir()) 
# files <- files |>
#   filter(starts_with("CME_MINI_NQ1!, 180"))
# files

df_og <- read_csv("hull_one_minute.csv", col_names = TRUE)
# df_og <- read_csv("hull_es.csv", col_names = TRUE)

# names <- colnames(df_og) 
# names <- sub("time", "datetime", names)
# names <- sub("Plot...6", "slow", names)
# names <- sub("Plot...7", "fast", names)
# colnames(df_og) <- names
df <- df_og
str(df)

# discern time interval from input file
first_row_time <- df$time[1] ; second_row_time <- df$time[2] ; third_row_time <- df$time[3]
interval <- min(as.numeric(difftime(second_row_time, first_row_time, units = "mins")),
                as.numeric(difftime(third_row_time, second_row_time, units = "mins")))
candles <- if(interval>60) sprintf("%.0f hrs", interval/60) else sprintf("%.0f mins", interval)
if(interval == 0) Warning("HOLY SHIT! WE HAVE REACHED THE END OF TIME!")

start_date <- min(df$time, na.rm=TRUE) 
end_date <- max(df$time, na.rm=TRUE)
date_range <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25

trades_global = tibble() 
results <- tibble() # create the file to collect the results of each run
epoch <- paste0(get_month(start_date),"-", get_day(start_date), "-",
                get_year(start_date)-2000," to ", get_month(end_date), 
                "-", get_day(end_date), "-", get_year(end_date)-2000)

n_fast <- 5
n_slow <- 8

start_value <- 1162  # required overnight margin in points
skid <- 0   # skid is expected loss on trade execution, set to ZERO for testing!
runs <- expand.grid(lag = seq(n_fast, n_slow, 1))

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "lm")

######################## EMA optimization sequence ########################

# for (j in seq_len(nrow(runs))) {
j <- 1

  # calculate HMA, Hull Moving Avg, and ATR, average true range
  
  df <- df_og |>
    select(time:close, Volume) |>
    mutate(h_yc = high - lag(close),
           yc_l = lag(close) - low,
           range = high - low) |>
    mutate(ATR = pmax(range, h_yc, yc_l, na.rm = TRUE)) |>
    mutate(fast = HMA(close, n = n_fast))
df$slow <- ewmaRcpp(df$close, n_slow)  
df <- df |>
  mutate(cross = fast - slow)

           # slow = HMA(close, n = n_slow),
           # 
  # create trades from signals
  df <- df |>
    mutate(on = if_else(cross > 0 & lag(cross) < 0, 1, 0), 
           off = if_else(cross < 0 & lag(cross) > 0, -1, 0),
           signal = on + off)
  
  # drop first 25 rows to let HMA `warm up`
  df <-  slice(df, 26:n())  
  
  df$on[1] <-  if_else(df$cross[1] > 0, 1, 0)  # catch first row signal, if there
  
  df <- df|>                         # trade details 
    mutate(signal = on + off,
           buy_date = if_else(on == 1, as_datetime(lead(time), tz="America/New_York"), NA),
           sell_date = if_else(off == -1, as_datetime(lead(time), tz="America/New_York"), NA),
           buy_price = if_else(on == 1, close + skid, NA),
           sell_price = if_else(off == -1, close - skid, 0),
           buy_amount = 1,             # no work on trade sizing yet, important though it is
           open_trade = cumsum(signal)) |>
    fill(buy_price) |>
    fill(buy_date)  |>
    drop_na(buy_price)
  
  if(df$on[nrow(df)] == 1) {     # no new trades in last period
    df$on[nrow(df)] = 0 ; df$signal[nrow(df)] = 0
  } else if (df$cross[nrow(df)] >0 | df$signal[nrow(df)] == -1) { # close out trade if long at EOF
    df$off[nrow(df)] <- -1
    df$signal[nrow(df)] <- -1
    df$open_trade[nrow(df)] <- 0
    df$sell_date[nrow(df)] <- df$time[nrow(df)]
    df$sell_price[nrow(df)] <- df$close[nrow(df)] - skid       
  }
  
  df <- df |>
    mutate(
      trade_pnl = if_else(signal == -1, (sell_price - buy_price) * buy_amount, 0),
      win_lose = as.factor(if_else(trade_pnl>0, "winner", "loser")),
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
  
  trades <- trades |>
    mutate(MAE = map_dbl(1:n(), ~ {
      row <- .x
      df |>
        filter(time >= trades$buy_date[row], time <= trades$sell_date[row]) |>
        pull(low) |>
        min(na.rm = TRUE)
    }), .after=win_lose,
    MAE_mark = MAE - buy_price,
    MAE_percent = (MAE / buy_price) - 1,
    trade_pnl_percent = trade_pnl / buy_price)
  
  # risk and return calculation
  end_value <- df$equity[nrow(df)] 
  end_val <- end_value / 1000000
  ratio <- end_value/ start_value
  ICAGR <- if(ratio <= 0) 0 else log(ratio)/ date_range
  drawdown <- max(df$drawdown)
  lake <- sum(df$lake) / sum(df$equity)
  bliss <- ICAGR / drawdown
  trade_count <- nrow(trades)
  trade_total_pnl <- sum(trades$trade_pnl)
  zz <- split_fun(trades, trade_pnl)
  wins <- zz[2,3] ; losses <- zz[1,3] ; won <- zz[2,2]; lost <- zz[1,2]
  win_rate <- wins / trade_count ; dollar_won <- -zz[2,4]/zz[1,4]
  results[j,1:17] <- as_tibble_row(          
    c(j=j, lag=lag, ICAGR=ICAGR, drawdown=drawdown, bliss=bliss, lake=lake, 
      end_value=end_value, end_val=end_val, trade_test=trade_test, 
      trade_count=trade_count, wins=wins, losses=losses, win_rate=win_rate,
      trade_total_pnl=trade_total_pnl, won=won, lost=lost, dollar_won=dollar_won),
    .name_repair = "universal")
  
  # accumulate the trades into the global trade table
  trade_tmp <- trades |>
    mutate(EMA = lag)
  trades_global <- trades_global |>
    bind_rows(trade_tmp)
  
  if(date_range < 1.1) {  # Killer graph: trades by EMA with equity and market
    df |>        #  only print for 1 yr or less data
      ggplot(aes(x = time)) +
      geom_ribbon(aes(ymin=low, ymax=high, x=time, fill = "band"), alpha = 0.9)+
      scale_fill_manual("", values="gray80") +
      geom_point(aes(x=time, y=close), shape=3, alpha=0.8) +
      geom_line(aes(x=time, y=lag), alpha=0.9) +
      geom_segment(data=trades, aes(x=buy_date, y=buy_price, xend=sell_date,
                                    yend=sell_price, color=factor(win_lose)), linewidth = 2) +
      scale_color_manual(values= c("red", "green3")) +
      scale_y_continuous(sec.axis=sec_axis(~ . *(max(df$high)/min(df$low))-(min(df$low)) )) +
      geom_line(aes(x=time, y=equity *(max(high)/min(low)) + min(low)-min(equity)), linewidth=2, alpha=0.7, color="deepskyblue") +
      labs(title=sprintf("NQ: EMA: %0.f, %.0f trades, ICAGR:, %.2f, bliss: %.2f, lake: %.2f", 
                         lag, nrow(trades), ICAGR, bliss, lake),
           subtitle=paste0(candles, "chart, ", round(date_range, 1), " yrs of data, ", epoch))+
      xlab("Date")+
      ylab("NQ") +
      theme(legend.position = "none")
    
    ggsave(paste0("output/run ", candles, "chart EMA ", lag, " ", epoch, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  }
  
  trades |>
    ggplot(aes(MAE_percent, trade_pnl_percent,  color=factor(win_lose))) +
    geom_point(shape=3, size=2,) +
    scale_color_manual(values= c("red","green3")) + 
    labs(title=sprintf("NQ: EMA: %0.f, %.0f trades, ICAGR: %.2f, bliss: %.2f, lake: %.2f, DD: %.2f", 
                       lag, nrow(trades), ICAGR, bliss, lake, drawdown),
         subtitle=paste0(candles, " chart, ", round(date_range, 1), " yrs of data, ", epoch))+
    xlab("Maximum Adverse Excursion") + 
    ylab("Trade P&L") +
    theme(legend.position = "none")
  
  
  ggsave(paste0("output/stop ", candles, " EMA ", lag, " ", epoch, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
  df |>        # lake over time with equity line
    ggplot(aes(x = time)) +
    geom_ribbon(aes(ymin=equity*20, ymax=highwater*20, x=time, fill = "band"), alpha = 0.9)+
    scale_color_manual("", values="grey12")+
    scale_fill_manual("", values="red") +
    geom_line(aes(y = highwater*20), size = 1, alpha = 0.6) +
    geom_line(aes(x=time, y=drawdown *max(highwater)*20/max(drawdown)), alpha=0.2) +
    labs(title=sprintf("Lake Ratio: %0.2f, %.0f trades, ICAGR: %.2f, Bliss: %.2f, DD: %.2f", 
                       lake, nrow(trades), ICAGR, bliss,  drawdown),
         subtitle=paste0(candles, " chart, EMA: ", lag,", ", epoch, " with unscaled drawdown in the background"),
         x="Year", y="Ending equity after $23k opening margin start") +
    scale_y_continuous(labels=scales::dollar_format(), limits = c(0,NA)) +
    theme(legend.position = "none")
  
  ggsave(paste0("output/lake over time ", candles, " EMA ", lag, " ", epoch, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
# }       #################### optimization loop end    ##########################
