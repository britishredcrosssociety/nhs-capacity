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
ae <- read_rds("preprocess/data/scotland_ae.rds")
bed_availability <- read_rds("preprocess/data/scotland_bed_availability.rds")
cancer <- read_rds("preprocess/data/scotland_cancer_waiting_times.rds")
dtoc <- read_rds("preprocess/data/scotland_delayed_transfer_of_care.rds")
rtt <- read_rds("preprocess/data/scotland_referral_to_treatment_waiting_times.rds")

combined <-
  ae |>
  left_join(bed_availability, by = "NHS Board") |>
  left_join(cancer, by = "NHS Board") |>
  left_join(dtoc, by = "NHS Board") |>
  left_join(rtt, by = "NHS Board")

# ---- Find worst-performing Trusts across all metrics ----
# Bin each metric into quintiles and look for HBs in the worst-performing
# quintile across multiple metrics
ranks <-
  combined |>
  mutate(
    ae_rank = inverse_rank(`Percentage seen within 4 hours`),
    beds_rank = inverse_rank(`Average number of available staffed beds`),
    cancer_rank = rank(`Average number of days waited from receipt of an urgent referral with suspicion of cancer to first cancer treatment (62 day standard)`),
    dtoc_rank = rank(`Bed days occupied by delayed discharge patients`),
    rtt_rank = inverse_rank(`Percentage seen within 18 weeks`)
  ) |>
  select(`NHS Board`, ends_with("_rank"))

bins <-
  ranks |>
  mutate(
    ae_bin = quantise(ae_rank, num_quantiles = 5),
    beds_bin = quantise(beds_rank, num_quantiles = 5),
    cancer_bin = quantise(beds_rank, num_quantiles = 5),
    dtoc_bin = quantise(dtoc_rank, num_quantiles = 5),
    rtt_bin = quantise(rtt_rank, num_quantiles = 5)
  ) |>
  select(`NHS Board`, ends_with("_bin"))

bins_sum <-
  bins |>
  rowwise() |>
  mutate(
    bin_sum = sum(
      c_across(!`NHS Board`),
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
      count_if_worst(beds_bin) +
      count_if_worst(cancer_bin) +
      count_if_worst(dtoc_bin) +
      count_if_worst(rtt_bin)
  )

performance <-
  bins_worst_count |>
  select(
    hb_name = `NHS Board`,
    `A&E performance (5 = worst performing)` = ae_bin,
    `Bed occupancy performance (5 = worst performing)` = beds_bin,
    `Cancer waiting list performance (5 = worst performing)` = cancer_bin,
    `DToC performance (5 = worst performing)` = dtoc_bin,
    `RTT performance (5 = worst performing)` = rtt_bin,
    `Overall performance (5 = worst performing)` = overall_bin,
    `No. of services (out of 5) scoring worst in performance` = sum_of_5s
  ) |>
  mutate(hb_name = str_replace_all(hb_name, "&", "and"))

# ---- Join boundaries ----
scotland_performance <-
  boundaries_hb |>
  left_join(performance, by = "hb_name")

use_data(scotland_performance, overwrite = TRUE)