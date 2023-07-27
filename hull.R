#  long-short exponential moving average crossover trading system
rm(list=ls())
library(tidyverse)
library(lubridate)
library(tidyquant)
library(Rcpp)
library(TTR)

R_path <- "Dropbox/Apps/R/projects/hull" ; path <- getwd()
if (!grep(R_path, path, ignore.case = TRUE))  setwd(R_path)

mvx_og <- read_csv("hull_es.csv", col_names = TRUE)

names <- colnames(mvx_og) 
names <- sub("time", "datetime", names)
names <- sub("Plot...6", "slow", names)
names <- sub("Plot...7", "fast", names)
colnames(mvx_og) <- names
mvx <- mvx_og
# str(mvx)
# glimpse(mvx)

mvx |>
  ggplot(aes(x = datetime, y = close)) +
  geom_point(size = 1, shape = 4, alpha = 0.1) +
  geom_smooth(method = "lm")

n_fast <- 16
n_slow <- 25

# calculate HMA, Hull Moving Avg, and ATR, average true range
mvx <- mvx_og |>
  select(datetime:fast) |>
  mutate(h_yc = high - lag(close),
         yc_l = lag(close) - low,
         range = high - low) |>
  mutate(ATR = pmax(range, h_yc, yc_l, na.rm = TRUE)) |>
  mutate(fast2 = HMA(close, n = n_fast),
         slow2 = HMA(close, n = n_slow),
         cross = fast2 - slow2)

# drop first 25 rows to let HMA `warm up`
mvx <-  slice(mvx, 26:n())  

# create trades from signals
mvx <- mvx |>
  mutate(on = ifelse(cross > 0 & lag(cross) < 0, 1, 0), 
         off = ifelse(cross < 0 & lag(cross) > 0, -1, 0),
         signal = on + off)

# buy trade details
mvx <- mvx |>
  mutate( buy_date = ifelse(on == 1, as_date(lead(datetime)), 0),
          buy_price = ifelse(on == 1, close, 0))

# sell trade details
mvx <- mvx |>
  mutate(sell_date = ifelse(off == -1, as_date(lead(datetime)), 0),
         sell_price = ifelse(off == -1, close, 0))

# Close out last trade if long when time (actually, data file) runs out
if (mvx$cross[nrow(mvx)] > 0) {
  mvx$off[nrow(mvx)] <- -1
  mvx$sell_date[nrow(mvx)] <- mvx$datetime[nrow(mvx)]
  mvx$sell_price[nrow(mvx)] <- mvx$close[nrow(mvx)] 
}

# Trade tables
buys <- mvx |>
  select(on:buy_price) |>
  filter(on == 1) |>
  select(starts_with("buy"))

sells <- mvx |>
  select(off:sell_price) |>
  filter(off == -1) |>
  select(starts_with("sell"))

if(nrow(sells) > nrow(buys)) {
  sells = slice(sells, 2:n())
}

trades <- bind_cols(sells, buys)
trades$buy_date <- as_datetime(as.numeric(trades$buy_date))
trades$sell_date <- as_datetime(as.numeric(trades$sell_date))

# calculate trade size based on pnl and book equity
trades <- trades |>
  mutate( closed_pnl = 0,    
          buy_amt = 1,
          pnl = 0,
          pnl_s = 0)

# calculate first row
trades$pnl[[1]] = (trades$sell_price[[1]] - trades$buy_price[[1]]) * 
  trades$buy_amt[[1]]

# calculate the rest of the trade table
for (i in 2:nrow(trades)){
  trades$closed_pnl[[i]] <- trades$closed_pnl[[i-1]] + trades$pnl[[i-1]]
  
  trades$pnl[[i]] <- (trades$sell_price[[i]] - trades$buy_price[[i]]) * 
    trades$buy_amt[[i]]
  
  trades$pnl_s[[i]] <- (trades$sell_price[[i-1]] - trades$buy_price[[i]]) 
}

print(sum(trades$pnl))
print(sum(trades$pnl_s))
answer <- sum(trades$pnl) + sum(trades$pnl_s)
print(answer)

# print(summary(trades$pnl))
# print(summary(trades$pnl_s))
# print(summary(trades$closed_pnl))


