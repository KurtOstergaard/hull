# viz2.R - risk and return scatterplots for prospecting moving average pairs

library(tidyverse)
library(ggrepel, quietly = TRUE)
options(ggrepel.max.overlaps = Inf)      # ggrepel options for ggplot2
theme_set(theme_light())                # ggplot theme or _bw()

# results <- results |>  filter(!fast_lag==slow_lag) # for same MA permutation issue
rez <- results |> slice_max(bliss, n=50)
  
rez |>         # labels for EMA numbers, little white boxes
  ggplot(aes(x = ICAGR, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
  geom_point(color="gray60") +
  geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                   min.segment.length=0, force=0.5, max.iter=10000) +
  labs(title=paste("Growth rate vs drawdowns"),
       subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                       max(runs$fast), " slow ",min(runs$slow), "-", 
                       max(runs$slow),", ", round(date_range, 0),
                       "D of data, ", epoch)) 
# coord_cartesian(xlim = c(1, NA))
ggsave(paste0(here("output", "risk ICAGR v DD "), run_id, run_time, ".pdf"), width=14, height=11, units="in", dpi=300)

rez |>
  ggplot(aes(x = lake, y = bliss, label = paste0(fast_lag, "-", slow_lag))) +
  geom_point(color="gray60") +
  geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                   min.segment.length=0, force=0.5, max.iter=10000) +
  labs(title=paste("Lake ratio vs bliss"),
       subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                       max(runs$fast), " slow ",min(runs$slow), "-", 
                       max(runs$slow),", ", round(date_range, 0),
                       "D of data, ", epoch)) 
ggsave(paste0(here("output", "risk lake v bliss "), run_id, run_time, ".pdf"), width=14, height=11, units="in", dpi=300)

rez |>
  ggplot(aes(x = lake, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
  geom_point(color="gray60") +
  geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                   min.segment.length=0, force=0.5, max.iter=10000) +
  labs(title=paste("Lake ratio vs drawdowns"),
       subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                       max(runs$fast), " slow ",min(runs$slow), "-", 
                       max(runs$slow),", ", round(date_range, 0),
                       "D of data, ", epoch))
ggsave(paste0(here("output", "risk lake v DD "), run_id, run_time, ".pdf"), width=14, height=11, units="in", dpi=300)

rez |>
  ggplot(aes(x = end_value, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
  geom_point(color="gray60") +
  geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                   min.segment.length=0, force=0.5, max.iter=10000) +
  scale_x_continuous(labels=scales::dollar_format()) +
  labs(title=paste("Ending value vs drawdowns"),
       subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                       max(runs$fast), " slow ",min(runs$slow), "-", 
                       max(runs$slow),", ", round(date_range, 0),
                       "D of data, ", epoch)) 
#    coord_cartesian(ylim = c(NA,1))
ggsave(paste0(here("output", "risk end value v DD "), run_id, run_time, ".pdf"), width=14, height=11, units="in", dpi=300)

rez |>
  ggplot(aes(x = ICAGR, y = lake, label = paste0(fast_lag, "-", slow_lag))) +
  geom_point(color="gray60") +
  geom_label_repel(label.padding=unit(0.1, "lines"), label.size=0.05, 
                   min.segment.length=0, force=0.5, max.iter=10000) +
  labs(title=paste("Growth rate vs lake ratio"),
       subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                       max(runs$fast), " slow ",min(runs$slow), "-", 
                       max(runs$slow),", ", round(date_range, 0),
                       "D of data, ", epoch)) 
# coord_cartesian(xlim = c(1, NA))
ggsave(paste0(here("output", "risk ICAGR v lake "), run_id, run_time, ".pdf"), width=14, height=11, units="in", dpi=300)

rez |>
  ggplot(aes(x = bliss, y = drawdown, label = paste0(fast_lag, "-", slow_lag))) +
  geom_point(color="gray60") +
  geom_label_repel(label.padding=unit(0.15, "lines"), label.size=0.05, 
                   min.segment.length=0, force=0.5, max.iter=10000) +
  labs(title=paste("Bliss vs drawdowns"),
       subtitle=paste0(candles, " periods, fast ", min(runs$fast), "-", 
                       max(runs$fast), " slow ",min(runs$slow), "-", 
                       max(runs$slow),", ", round(date_range, 0),
                       "D of data, ", epoch)) 
# coord_cartesian(ylim = c(NA,1))
ggsave(paste0(here("output", "risk bliss v DD "), run_id, run_time, ".pdf"), width=14, height=11, units="in", dpi=300)


