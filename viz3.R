#viz3.R - individual moving average pair graphs for examining trades and details

library(tidyverse)

#  only print for good ICAGR
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



