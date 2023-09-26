#  hull2.R  -- hull moving average  trading system
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
library(ggrepel, quietly = TRUE)
library(chron)
library(here)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

options(ggrepel.max.overlaps = Inf)      # ggrepel options for ggplot2
theme_set(theme_light())                # ggplot theme or _bw()
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

product <- "ES"  #####  <- Yes, name it here. ES or NQ or whatever
fast_seq <- 1500
slow_seq <- 3000
runs <- expand.grid(slow=seq(2400, slow_seq, 100), fast=seq(300, fast_seq, 100))

df_og <- read_csv("CME_MINI_ES1!, 1S_9e97e.csv", col_names = TRUE)

# discern time interval from input file
df <- df_og
first_row_time <- df$time[1] ; second_row_time <- df$time[2] ; third_row_time <- df$time[3]
interval <- min(as.numeric(difftime(second_row_time, first_row_time, units = "secs")),
                as.numeric(difftime(third_row_time, second_row_time, units = "secs")))
if(interval <= 0) Warning("HOLY SHIT! WE HAVE REACHED THE END OF TIME!") # file sorted backwards?

# candles is a string for in graphs
candles <- if(interval>=3600) {
  sprintf("%.0f hr", interval/3600) 
  } else if (interval>=60) {
    sprintf("%.0f min", interval/60)
  } else {
    sprintf("%.0f sec", interval)
  }
# time management for markets operating 23/6, need daily stop and start 
start_date <- min(df$time) 
end_date <- max(df$time)
date_range <- as.numeric(difftime(end_date, start_date, units = "days"))
dates_run <- floor_date(start_date, unit="days"):floor_date(end_date, unit = "days")
witching_hour <- date_time_build(get_year(start_date), get_month(start_date),
  get_day(start_date), 21L, 0L, 0L, zone="UTC")
last_call <- date_seq(add_seconds(witching_hour, -interval), to=end_date, by=86400)

the_high <- max(df$high)
the_low <- min(df$low)
the_range <- the_high - the_low
trades_global = tibble() 
results <- tibble() # create the file to collect the results of each run
epoch <- paste0(get_month(start_date),"-", get_day(start_date), "-",
                get_year(start_date)-2000," to ", get_month(end_date), 
                "-", get_day(end_date), "-", get_year(end_date)-2000)

start_value <- 1162  # required overnight margin in points
skid <- 0   # skid is expected loss on trade execution, set to ZERO for testing!

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) 
  # geom_smooth(method = "lm")


######################## optimization sequence #############################


for (j in seq_len(nrow(runs))) {
# j <- 98

  df <- df_og
  fast_lag <- runs$fast[j]
  slow_lag <- runs$slow[j]
  
  # if(fast_lag == slow_lag) {  # handle results record where fast=slow lag
  #   results[j,1:17] <- as_tibble_row(          
  #     c(j=j, fast_lag=fast_lag, slow_lag=slow_lag, ICAGR=0, drawdown=0, 
  #       bliss=0, lake=0, end_value=0, trade_test=0, 
  #       trade_count=0, wins=0, losses=0, win_rate=0,
  #       trade_total_pnl=0, won=0, lost=0, dollar_won=0),
  #     .name_repair = "universal")
  #   next
  # }
  
  # exponential moving averages
  df$Efast <- ewmaRcpp(df$close, fast_seq)
  # df$Eslow <- ewmaRcpp(df$close, slow_seq)
  df$Eslow <- ewmaRcpp(df$close, runs$slow[j])
  
  # calculate HMA, Hull Moving Avg, cross trade signal and ATR, average true range
  df <- df |>
    select(time:close, Volume, Efast, Eslow) |>
    mutate(time = as.POSIXct(time, tz = "UTC"), # Ensure time is in POSIXct format
           h_yc = high - lag(close),
           yc_l = lag(close) - low,
           range = high - low, 
    ATR = pmax(range, h_yc, yc_l, na.rm = TRUE),
    fast = HMA(close, fast_lag),
    slow = (HMA(close, slow_lag) +1e-6), 
    dslow = if_else(slow-lag(slow)==0, 1e-7, slow-lag(slow)),
    cross = fast - Eslow,
    # cross = dslow,    #  cross = fast - slow,
    on = if_else(cross > 0 & lag(cross) < 0, 1, 0), 
    off = if_else(cross < 0 & lag(cross) > 0, -1, 0),
    signal = on + off) |>
    drop_na(on) |>
    drop_na(off)
    
  df$off[1] <- 0
  
  # close and restart trades on market schedule 23/6
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
    mutate(EMA_slow = slow_lag,
           EMA_fast = fast_lag)
  trades_global <- trades_global |>
    bind_rows(trade_tmp)

    trades_w <- trades |>
    filter(buy_date >"2023-09-06" & buy_date < "2023-09-07") 
    
    #  only print for good ICAGR
  if(ICAGR > 10) {  # graph trades by EMA with equity and market
    df |>        
      # filter(time >"2023-09-06" & time < "2023-09-07") |>
      ggplot(aes(x = time)) +
      geom_line(aes(x=time, y=slow), alpha=0.6) +
      geom_ribbon(aes(ymin=low, ymax=high, x=time, fill = "band"), alpha = 0.9)+
      scale_fill_manual("", values="gray80") +
      geom_point(aes(x=time, y=close), shape=3, alpha=0.8) +
      geom_segment(data=trades_w, aes(x=buy_date, y=buy_price, xend=sell_date,
              yend=sell_price, color=factor(win_lose)), linewidth = 2) +
      scale_color_manual(values= c("red", "green3")) +
      labs(title=sprintf("%s: fast: %0.f slow: %0.f, %.0f trades, ICAGR:, %.2f, bliss: %.2f, lake: %.2f", 
                         product, fast_lag, slow_lag, nrow(trades), ICAGR, bliss, lake),
           subtitle=paste0(candles, "chart, ", round(date_range, 0), "D of data, ", epoch))+
      xlab("Date")+
      ylab(product) +
      theme(legend.position = "none")
    
    ggsave(paste0(here("output", "run "), candles," ", runs$fast[j], 
                  "-", runs$slow[j], " ", epoch, run_time, " ", j, ".pdf"), 
           width=10, height=8, units="in", dpi=300)

      # scale_y_continuous(sec.axis=sec_axis(~ . *(max(df$high)/min(df$low))-(min(df$low)) )) +
      # geom_line(aes(x=time, y=equity *(max(high)/min(low)) + min(low)-min(equity)), linewidth=2, alpha=0.7, color="deepskyblue") +

    trades |>   # Stop analysis
    ggplot(aes(MAE_percent, trade_pnl_percent,  color=factor(win_lose))) +
    geom_point(shape=3, size=2,) +
    scale_color_manual(values= c("red","green3")) + 
    labs(title=sprintf("%s: EMA: %0.f, %.0f trades, ICAGR: %.2f, bliss: %.2f, lake: %.2f, DD: %.2f", 
                       product, slow_lag, nrow(trades), ICAGR, bliss, lake, drawdown),
         subtitle=paste0(candles, " chart, ", round(date_range, 0), "D of data, ", epoch))+
    xlab("Maximum Adverse Excursion") + 
    ylab("Trade P&L") +
    theme(legend.position = "none")
  
  ggsave(paste0(here("output", "stop "), candles," ", runs$fast[j], 
                "-", runs$slow[j], " ", epoch, run_time, " ", j, ".pdf"), 
         width=10, height=8, units="in", dpi=300)
    
  df |>        # lake over time with equity line
    ggplot(aes(x = time)) +
    geom_ribbon(aes(ymin=equity, ymax=highwater, x=time, fill = "band"), alpha = 0.9)+
    scale_color_manual("", values="grey12")+
    scale_fill_manual("", values="red") +
    geom_line(aes(y = highwater), size = 1, alpha = 0.6) +
    geom_line(aes(x=time, y=drawdown *max(highwater)/max(drawdown)), alpha=0.2) +
    labs(title=sprintf("Lake Ratio: %0.2f, %.0f trades, ICAGR: %.2f, Bliss: %.2f, DD: %.2f", 
                       lake, nrow(trades), ICAGR, bliss,  drawdown),
         subtitle=paste0(candles, " chart, EMA: ", slow_lag,", ", epoch, " with unscaled drawdown in the background"),
         x="Year", y="Ending equity in points, 1162 opening margin") +
    scale_y_continuous(labels=scales::dollar_format(), limits = c(0,NA)) +
    theme(legend.position = "none")
  
  ggsave(paste0(here("output", "lake over time "), candles," ", runs$fast[j], 
                "-", runs$slow[j], " ", epoch, run_time, " ", j, ".pdf"), 
         width=10, height=8, units="in", dpi=300)
  } # if printing statement close
    

    
}       #################### optimization loop end    ##########################

    
  
  # save the results and trades_global files
  run_id <- paste0( " ", candles," fast ", min(runs$fast), "-", max(runs$fast), " slow ",min(runs$slow),
                    "-", max(runs$slow), " fr ", epoch)
  results_file_name <- paste0(here("output", "results "), run_id, run_time, ".csv", sep="")
  write_csv(results, results_file_name)
  trade_file_name <- paste0(here("output", "trades "), run_id, run_time, ".csv", sep="")
  write_csv(trades_global, trade_file_name)
  
  end_time <- Sys.time() ;forever <- end_time - start_time
  secs <- forever  / nrow(runs)
  sprintf("Yo, %1.2f min,  %1.2f per run, %i runs, %s records, over %1.2f days of data", 
          forever, secs, nrow(results), format(nrow(df), big.mark=","), date_range)
  
  
  ##########################
  
  df |>
    ggplot(aes(x = time, y = close)) +
    geom_line(alpha = 0.4) +
    geom_smooth(method = "lm") +
    geom_line(aes(y=fast, alpha = 0.2)) +
    geom_line(aes(y=slow, alpha = 0.2)) +
    geom_line(aes(y=Efast, alpha = 0.2)) +
    geom_line(aes(y=Eslow, alpha = 0.2)) 
    
    
  ####################   risk and return optimization scatterplots
  # results <- results |>
  #   filter(!fast_lag==slow_lag)
  
  results |>         # labels for EMA numbers, little white boxes
    ggplot(aes(x = ICAGR, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
    geom_path(color="gray60") +
    geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                     min.segment.length=0, force=0.5, max.iter=10000) +
    labs(title=paste("Growth rate vs drawdowns"),
         subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
               max(runs$fast), " slow ",min(runs$slow), "-", 
               max(runs$slow),", ", round(date_range, 0),
                         "D of data, ", epoch)) 
    # coord_cartesian(xlim = c(1, NA))
  ggsave(paste0(here("output", "risk ICAGR v DD "), run_id, run_time, ".pdf"), width=14, height=11, units="in", dpi=300)
  
  results |>
    ggplot(aes(x = lake, y = bliss, label = paste0(fast_lag, "-", slow_lag))) +
    geom_path(color="gray60") +
    geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                     min.segment.length=0, force=0.5, max.iter=10000) +
    labs(title=paste("Lake ratio vs bliss"),
         subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                         max(runs$fast), " slow ",min(runs$slow), "-", 
                         max(runs$slow),", ", round(date_range, 0),
                         "D of data, ", epoch)) 
  ggsave(paste0(here("output", "risk lake v bliss "), run_id, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
  results |>
    ggplot(aes(x = lake, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
    geom_path(color="gray60") +
    geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                     min.segment.length=0, force=0.5, max.iter=10000) +
    labs(title=paste("Lake ratio vs drawdowns"),
         subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                         max(runs$fast), " slow ",min(runs$slow), "-", 
                         max(runs$slow),", ", round(date_range, 0),
                         "D of data, ", epoch))
  ggsave(paste0(here("output", "risk lake v DD "), run_id, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
  results |>
    ggplot(aes(x = end_value, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
    geom_path(color="gray60") +
    geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                     min.segment.length=0, force=0.5, max.iter=10000) +
    scale_x_continuous(labels=scales::dollar_format()) +
    labs(title=paste("Ending value vs drawdowns"),
         subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                         max(runs$fast), " slow ",min(runs$slow), "-", 
                         max(runs$slow),", ", round(date_range, 0),
                         "D of data, ", epoch)) 
#    coord_cartesian(ylim = c(NA,1))
  ggsave(paste0(here("output", "risk end value v DD "), run_id, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
  # geom_smooth(method = "lm")
  
  results |>
    ggplot(aes(x = ICAGR, y = lake, label = paste0(fast_lag, "-", slow_lag))) +
    geom_path(color="gray60") +
    geom_label_repel(label.padding=unit(0.1, "lines"), label.size=0.05, 
                     min.segment.length=0, force=0.5, max.iter=10000) +
    labs(title=paste("Growth rate vs lake ratio"),
         subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                         max(runs$fast), " slow ",min(runs$slow), "-", 
                         max(runs$slow),", ", round(date_range, 0),
                         "D of data, ", epoch)) 
    # coord_cartesian(xlim = c(1, NA))
  ggsave(paste0(here("output", "risk ICAGR v lake "), run_id, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
  results |>
    ggplot(aes(x = bliss, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
    geom_path(color="gray60") +
    geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                     min.segment.length=0, force=0.5, max.iter=10000) +
    labs(title=paste("Bliss vs drawdowns"),
         subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                         max(runs$fast), " slow ",min(runs$slow), "-", 
                         max(runs$slow),", ", round(date_range, 0),
                         "D of data, ", epoch)) 
  # coord_cartesian(ylim = c(NA,1))
  ggsave(paste0(here("output", "risk bliss v DD "), run_id, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
  
  
  library(plotly)
  yo_fig <- results |>
    select(slow_lag, fast_lag, ICAGR) |>
    pivot_wider(names_from = slow_lag, values_from = ICAGR) |>
    as.matrix()
  rownames(yo_fig) <- yo_fig[,1]
  yo_fig <- yo_fig[,-1]
  
  plot_ly(z = ~yo_fig) |> add_surface()

  # row_names <- sort(unique(yo_icagr[,1]))
    
  # yo_fig[yo_fig < -1] <- -1
    
    
    
    plot_ly(z = ~yo_icagr) |> add_surface(
    contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
  )
  yo_icagr
  
  # volcano is a numeric matrix that ships with R
  fig <- plot_ly(z = ~volcano)
  fig <- fig %>% add_surface()
  fig
  
fig2 <-  results |>
  select(fast_lag, slow_lag, ICAGR) |>
  pivot_wider(names_from = slow_lag, values_from = ICAGR)|>
  as.matrix() 
    plot_ly(z = ~fig2) |>
    add_surface()
  
  fig2
  
  
  # Basic 3D surface plot
  library(plotly)
  # volcano is a numeric matrix that ships with R
  fig <- plot_ly(z = ~vol3)
  fig <- fig %>% add_surface()
  fig
  
  # df |> 
  #   ggplot(aes(x = time)) +
  #   geom_line(aes(y=drawdown)) +
  #   # geom_line(aes(y=equity * max(drawdown)/max(equity)), color="blue") +
  #   geom_line(aes(x=time, y=real_close *max(drawdown)/max(real_close) ), alpha=1, color="gray80") +
  #   labs(title=paste("Drawdowns over time with market price overlay"),
  #        subtitle=paste0(candles, " periods, EMA: ", EMA_low, "-", EMA_high,", ",
  #                        round(date_range, 0), "D of data, ", epoch))
  # ggsave(paste0("output/DD over time ", run_id, run_time, ".pdf"), width=10, height=8, units="in", dpi=300)
  
