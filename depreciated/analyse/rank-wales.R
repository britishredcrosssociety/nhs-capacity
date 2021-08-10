library(tidyverse)

source("https://github.com/britishredcrosssociety/resilience-index/raw/main/R/utils.R")  # for quantise()

# ---- Load funs ----
# 1 = best
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

# ---- Load data ----
ae <- read_csv("data/wales-ae.csv")
ambo <- read_csv("data/wales-ambo.csv")
beds <- read_csv("data/wales-beds-monthly.csv")
cancer <- read_csv("data/wales-cancer.csv")
rtt <- read_csv("data/wales-rtt.csv")

lhb <- tribble(
  ~HB_code, ~HB_code_long, ~Hospital_code, ~HB_name,
  "7A1", "W11000023", "7A1W11000023", "Betsi Cadwaladr University Local Health Board",
  "7A2", "W11000025", "7A2W11000025", "Hywel Dda University Local Health Board",
  "7A3", "W11000031", "7A3W11000031", "Swansea Bay University Local Health Board",
  "7A4", "W11000029", "7A4W11000029", "Cardiff and Vale University Local Health Board",
  "7A5", "W11000030", "7A5W11000030", "Cwm Taf Morgannwg University Local Health Board",
  "7A6", "W11000028", "7A6W11000028", "Aneurin Bevan University Local Health Board",
  "7A7", "W11000024", "7A7W11000024", "Powys Teaching Local Health Board"
)

wales <- 
  lhb |> 
  left_join(
    ae |> select(Hospital_code, `Percentage non-breached` = Percentages) |> mutate(`Percentage breached` = 100 - `Percentage non-breached`), 
    by = "Hospital_code"
  ) |> 
    
  left_join(
    ambo |> select(-Date, -HB),
    by = "HB_code"
  ) |> 

  left_join(
    beds |> select(HB_code, `% general and acute beds occupied`),
    by = "HB_code"
  ) |> 
  
  left_join(
    cancer |> select(-Date, -HB),
    by = "HB_code"
  ) |> 
  
  left_join(
    rtt |> select(-HB),
    by = c("HB_code_long" = "HB_code")
  )

# ---- Find worst-performing Health Boards across all metrics ----
# Bin each metric into quintiles and look for HBs in the worst-performing quintile across multiple metrics
wales_performance <- 
  wales |> 
  mutate(
    ae_bin = quantise(`Percentage breached`, num_quantiles = 5, highest_quantile_worst = TRUE),
    ambo_bin = quantise(`Red calls - % of emergency responses arriving at the scene within 8 minutes`, num_quantiles = 5, highest_quantile_worst = FALSE),
    beds_bin = quantise(`% general and acute beds occupied`, num_quantiles = 5, highest_quantile_worst = TRUE),
    cancer_bin = quantise(`% starting treatment within 62 days`, num_quantiles = 5, highest_quantile_worst = FALSE),
    rtt_bin = quantise(`% waiting 53+ weeks`, num_quantiles = 5, highest_quantile_worst = TRUE)
  ) |> 
  
  # Calculate overall Trust performance, based on the quintiles
  rowwise() |>
  mutate(bin_sum = sum(c_across(ae_bin:rtt_bin), na.rm = TRUE)) |> 
  ungroup() |> 
  
  mutate(
    sum_of_5s = count_if_worst(ae_bin) +
      count_if_worst(ambo_bin) +
      count_if_worst(beds_bin) +
      count_if_worst(cancer_bin) +
      count_if_worst(rtt_bin)
  ) |> 
  
  arrange(desc(sum_of_5s))

wales_performance |> 
  rename(
    `Health Board` = HB_name,
    `A&E performance (5 = worst performing 20%)` = ae_bin,
    `Ambulance performance (5 = worst performing 20%)` = ambo_bin,
    `Bed occupancy performance (5 = worst performing 20%)` = beds_bin,
    `Cancer waiting list performance (5 = worst performing 20%)` = cancer_bin,
    `RTT performance (5 = worst performing 20%)` = rtt_bin
  ) |> 
  select(-HB_code, -HB_code_long, -Hospital_code, -bin_sum, -sum_of_5s) |> 
  
  write_csv(file = "data/nhs-performance-wales.csv")
