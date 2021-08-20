# ---- Load libs ----
library(tidyverse)
library(usethis)
library(geographr)
library(sf)

# ---- Load funs ----
source("https://github.com/britishredcrosssociety/resilience-index/raw/main/R/utils.R")

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

# ---- Load data ----
ae <- read_rds("preprocess/data/wales_ae.rds")
ambulance <- read_rds("preprocess/data/wales_ambulance_services.rds")
beds <- read_rds("preprocess/data/wales_bed_availability.rds")
cancer <- read_rds("preprocess/data/wales_cancer_waiting_times.rds")
rtt <- read_rds("preprocess/data/wales_referral_treatment_waiting_times.rds")

lookup <-
  tribble(
    ~HB_code, ~HB_code_long, ~Hospital_code, ~`Health Board`,
    "7A1", "W11000023", "7A1W11000023", "Betsi Cadwaladr University Local Health Board",
    "7A2", "W11000025", "7A2W11000025", "Hywel Dda University Local Health Board",
    "7A3", "W11000031", "7A3W11000031", "Swansea Bay University Local Health Board",
    "7A4", "W11000029", "7A4W11000029", "Cardiff and Vale University Local Health Board",
    "7A5", "W11000030", "7A5W11000030", "Cwm Taf Morgannwg University Local Health Board",
    "7A6", "W11000028", "7A6W11000028", "Aneurin Bevan University Local Health Board",
    "7A7", "W11000024", "7A7W11000024", "Powys Teaching Local Health Board"
  )

combined <-
  lookup |>
  left_join(
    ae |>
    select(Hospital_code, `Percentage non-breached` = Percentages) |>
    mutate(`Percentage breached` = 100 - `Percentage non-breached`),
    by = "Hospital_code"
  ) |>
  left_join(
    ambulance |> select(-HB),
    by = "HB_code"
  ) |>
  left_join(
    beds |> select(HB_code, `% general and acute beds occupied`),
    by = "HB_code"
  ) |>
  left_join(
    cancer |> select(-HB),
    by = "HB_code"
  ) |>
  left_join(
    rtt |> select(-HB),
    by = c("HB_code_long" = "HB_code")
  )

# ---- Find worst-performing Trusts across all metrics ----
# Bin each metric into quintiles and look for HBs in the worst-performing
# quintile across multiple metrics
ranks <-
  combined |>
  mutate(
    ae_rank = rank(`Percentage breached`),
    ambulance_rank = inverse_rank(`Red calls - % of emergency responses arriving at the scene within 8 minutes`),
    beds_rank = rank(`% general and acute beds occupied`),
    cancer_rank = inverse_rank(`% starting treatment within 62 days`),
    rtt_rank = rank(`% waiting 53+ weeks`)
  ) |>
  select(`Health Board`, ends_with("_rank"))

bins <-
  ranks |>
  mutate(
    ae_bin = quantise(ae_rank, num_quantiles = 5),
    ambulance_bin = quantise(ambulance_rank, num_quantiles = 5),
    beds_bin = quantise(beds_rank, num_quantiles = 5),
    cancer_bin = quantise(beds_rank, num_quantiles = 5),
    rtt_bin = quantise(rtt_rank, num_quantiles = 5)
  ) |>
  select(`Health Board`, ends_with("_bin"))

bins_sum <-
  bins |>
  rowwise() |>
  mutate(
    bin_sum = sum(
      c_across(!`Health Board`),
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  mutate(
    overall_rank = rank(bin_sum),
    overall_bin = quantise(overall_rank, num_quantiles = 5)
  ) |>
  arrange(desc(overall_bin))

bins_worst_count <-
  bins_sum |>
  mutate(
    sum_of_5s = count_if_worst(ae_bin) +
      count_if_worst(ambulance_bin) +
      count_if_worst(beds_bin) +
      count_if_worst(cancer_bin) +
      count_if_worst(rtt_bin)
  )

performance <-
  bins_worst_count |>
  select(
    lhb_name = `Health Board`,
    `A&E performance (5 = worst performing)` = ae_bin,
    `Ambulance performance (5 = worst performing)` = ambulance_bin,
    `Bed occupancy performance (5 = worst performing)` = beds_bin,
    `Cancer waiting list performance (5 = worst performing)` = cancer_bin,
    `RTT performance (5 = worst performing)` = rtt_bin,
    `Overall performance (5 = worst performing)` = overall_bin,
    `No. of services (out of 5) scoring worst in performance` = sum_of_5s
  ) |>
  mutate(
    lhb_name = str_replace_all(
      lhb_name,
      "Local Health Board",
      "Health Board"
    )
  )

# ---- Join boundary data ----
wales_performance <-
  boundaries_lhb |>
  left_join(performance)

use_data(wales_performance, overwrite = TRUE)