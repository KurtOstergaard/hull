# viz.R visualizations for the hull.R model 
library(tidyverse)
library(plotly)
library(processx)
library(here)

run_id <- paste0( " ", product," ", candles," fast ", min(runs$fast), "-", max(runs$fast), " slow ",min(runs$slow),
                  "-", max(runs$slow), "  ", epoch)

# Prospecting surface graph for comparing two moving average ranges
# 3D surface plot of two moving average pair's resulting growth rate
res1 <- results |>
  select(slow_lag, fast_lag, ICAGR) |>
  pivot_wider(names_from = slow_lag, values_from = ICAGR) |>
  as.matrix()
rownames(res1) <- res1[,1]
res1 <- res1[,-1]
fig1 <- plot_ly(x = ~colnames(res1), y = ~rownames(res1),  z = ~res1) |>
  add_surface(contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)))) |>
  layout(title = 'Moving Average Growth Rates',
         scene = list(
           xaxis = list(title = 'EMA slow moving avg'),
           yaxis = list(title = 'Hull fast moving avg'),
           zaxis = list(title = "ICAGR")))
fig1  
fig1_file_name <- paste0(here("output", "surface growth "), run_id, run_time, ".pdf", sep="")
save_image(fig1, fig1_file_name)

res2 <- results |>
  select(slow_lag, fast_lag, bliss) |>
  pivot_wider(names_from = slow_lag, values_from = bliss) |>
  as.matrix()
rownames(res2) <- res2[,1]
res2 <- res2[,-1]
fig2 <- plot_ly(x = ~colnames(res2), y = ~rownames(res2),  z = ~res2) |>
  add_surface(contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)))) |>
  layout(title = 'Moving Average Bliss',
         scene = list(
                 xaxis = list(title = 'EMA slow moving avg'),
                 yaxis = list(title = 'Hull fast moving avg'),
                 zaxis = list(title = "Bliss")))
fig2
fig2_file_name <- paste0(here("output", "surface bliss "), run_id, run_time, ".pdf", sep="")
save_image(fig2, fig2_file_name)

res3 <- results |>
  mutate(dry_lake = (1 -lake)) |>
  select(slow_lag, fast_lag, dry_lake) |>
  pivot_wider(names_from = slow_lag, values_from = dry_lake) |>
  as.matrix()
rownames(res3) <- res3[,1]
res3 <- res3[,-1]
fig3 <- plot_ly(x = ~colnames(res3), y = ~rownames(res3),  z = ~res3) |>
  add_surface(contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)))) |>
  layout(title = 'Moving Average Lake Ratio',
         scene = list(
           xaxis = list(title = 'EMA slow moving avg'),
           yaxis = list(title = 'Hull fast moving avg'),
           zaxis = list(title = "Lake")))
fig3
fig3_file_name <- paste0(here("output", "surface lake "), run_id, run_time, ".pdf", sep="")
save_image(fig3, fig3_file_name)


res4 <- results |>
  mutate(remaining = (1 -drawdown)) |>
  select(slow_lag, fast_lag, remaining) |>
  pivot_wider(names_from = slow_lag, values_from = remaining) |>
  as.matrix()
rownames(res4) <- res4[,1]
res4 <- res4[,-1]
fig4 <- plot_ly(x = ~colnames(res4), y = ~rownames(res4),  z = ~res4) |>
  add_surface(contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)))) |>
  layout(title = 'Moving Average Remaining (1-DD)',
         scene = list(
           xaxis = list(title = 'EMA slow moving avg'),
           yaxis = list(title = 'Hull fast moving avg'),
           zaxis = list(title = "Drawdown")))
fig4
fig4_file_name <- paste0(here("output", "surface drawdown "), run_id, run_time, ".pdf", sep="")
save_image(fig4, fig4_file_name)


# res[res < -1] <- -1   # adds a floor for values

