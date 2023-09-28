#viz3.R - individual moving average pair graphs for examining trades and details
library(tidyverse)
library(lubridate)
library(here)

process_csv_file <- function(file_path) {
  data <- read_csv(file_path, show_col_types = FALSE)
  
  num_rows <- nrow(data)
  file_name <- basename(file_path)
  return(tibble(file_name, num_rows))
}  

#   ################ find files  ############
# csv_files <- list.files(path=here("output"), pattern = "\\.csv$", full.names = TRUE)
csv_files <- list.files(path=here("output"), pattern = "^trades.", full.names = TRUE)
trade_files <- lapply(csv_files, process_csv_file) |>
  bind_rows()
colnames(trade_files) <- c("File Name", "Rows")
View(trade_files)
#   ################ load files  ############
big_trades <- read_csv(trade_file_name, col_names = TRUE)
# big_trades <- read_csv(here("output", "trades 1 runs 1 sec fast 580-580 slow 2680-2680 fr 9-18-23 to 9-19-23 21-51.csv"), col_names = TRUE)

mkt <- df_og |> select(time:close)
# mkt <- read_csv("CME_MINI_ES1!, 1S_9e97e.csv", col_names = TRUE) |>  select(time:close)


mkt_high <- max(mkt$high) ; mkt_low <- min(mkt$low) ; mkt_range <- mkt_high - mkt_low
fast_MA <- 19
slow_MA <- 3

these_trades <- big_trades |>
  filter(MA_fast == fast_MA) |>
  filter(MA_slow == slow_MA)

pnl <- sum(these_trades$trade_pnl)

mkt |>     
  ggplot(aes(x = time, y=close), alpha=0.6) +
  geom_line() +
  geom_ribbon(aes(ymin=low, ymax=high, x=time, fill = "band"), alpha = 0.4)+
  scale_fill_manual("", values="gray80") +
  geom_segment(data=these_trades, aes(x=buy_date, y=buy_price, xend=sell_date,
                                  yend=sell_price, color=factor(win_lose)), linewidth = 2) +
  scale_color_manual(values= c("red", "green3")) +
  labs(title=sprintf("%s: fast: %0.f slow: %0.f, %.0f trades", product, fast_MA, slow_MA, nrow(these_trades)),
       subtitle=paste0(candles, "chart, ", round(date_range, 0), "D of data, ", epoch))+
  xlab("Date")+
  ylab(product) +
  theme(legend.position = "none")




df |>        
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



