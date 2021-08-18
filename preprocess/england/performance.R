# ---- Load libs ----
library(tidyverse)
library(usethis)
library(geographr)
library(sf)

# ---- Load funs ----
source("https://github.com/britishredcrosssociety/resilience-index/raw/main/R/utils.R")

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

# Inverse rank - override imported to make NA's last
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = "keep")

# ---- Load data ----
ae <- read_rds("preprocess/data/england_ae.rds")
ambulance <- read_rds("preprocess/data/england_ambulance_quality_indicators.rds")
beds <- read_rds("preprocess/data/england_bed_occupancy.rds")
cancer <- read_rds("preprocess/data/england_cancer_wait_times.rds")
diagnostic <- read_rds("preprocess/data/england_diagnostic_wait_times.rds")
outpatient <- read_rds("preprocess/data/england_outpatient_referrals.rds")
rtt <- read_rds("preprocess/data/england_referral_treatment_waiting_times.rds")

stp_codes <-
  read_sf("https://opendata.arcgis.com/datasets/08d5070b4f324560aeef857e26701f77_0.geojson") |>
  st_drop_geometry() |>
  select(stp_code = STP20CD, stp_code_short = STP20CDH)

# Create STP lookup
lookup <-
  lookup_trust_stp |>
  filter(status == "open") |>
  select(`Trust Code` = nhs_trust_code, stp_code_short = stp_code) |>
  left_join(stp_codes) |>
  select(-stp_code_short)

# ---- Preprocess data ----
ae_processed <-
  ae |>
  select(`Trust Code`, `% Total <= 4 hours`) |>
  drop_na()

beds_night_processed <-
  beds |>
  select(`Trust Code`, `% Total Night Beds Occupied`) |>
  drop_na()

beds_day_processed <-
  beds |>
  select(`Trust Code`, `% Total Day Beds Occupied`) |>
  drop_na()

cancer_processed <-
  cancer |>
  filter(Standard == "62 Days") |>
  mutate(`% Breaches` = Breaches / `Total Treated`) |>
  select(`Trust Code`, `% Breaches`) |>
  drop_na()

diagnostic_processed <-
  diagnostic |>
  select(`Trust Code`, `% waiting 13+ weeks`) |>
  drop_na()

rtt_processed <-
  rtt |>
  filter(`Referral Treatment Type` == "Incomplete Pathways") |>
  select(`Trust Code`, `% Waiting 52+ Weeks`) |>
  drop_na()

# ---- Combine -----
combined <-
  lookup |>
  left_join(ae_processed) |>
  left_join(beds_night_processed) |>
  left_join(beds_day_processed) |>
  left_join(cancer_processed) |>
  left_join(diagnostic_processed) |>
  left_join(rtt_processed)

# ---- Remove Missing Data ----
# Count the number of NA's across each row
missing_count <-
  combined |>
  drop_na(stp_code) |>
  rowwise() |>
  mutate(
    number_missing = sum(
      is.na(
        c_across(!c(`Trust Code`, stp_code))
      )
    )
  ) |>
  ungroup()

# Drop rows containing data missing from more than two indicators so that STP's
# can be compared.
complete_data <-
  missing_count |>
  filter(
    number_missing == 0 |
      number_missing == 1
  ) |>
  select(-number_missing)

# ---- Find worst-performing Trusts across all metrics ----
# Bin each metric into quintiles and look for HBs in the worst-performing
# quintile across multiple metrics
ranks <-
  complete_data |>
  mutate(
    ae_rank = inverse_rank(`% Total <= 4 hours`),
    beds_night_rank = rank(`% Total Night Beds Occupied`, na.last = "keep"),
    beds_day_rank = rank(`% Total Day Beds Occupied`, na.last = "keep"),
    cancer_rank = rank(`% Breaches`, na.last = "keep"),
    diagnostic_rank = rank(`% waiting 13+ weeks`, na.last = "keep"),
    rtt_rank = rank(`% Waiting 52+ Weeks`, na.last = "keep")
  ) |>
  select(
    `Trust Code`,
    stp_code,
    ends_with("_rank")
  )

bins <-
  ranks |>
  mutate(
    ae_bin = quantise(ae_rank, num_quantiles = 5),
    beds_night_bin = quantise(beds_night_rank, num_quantiles = 5),
    beds_day_bin = quantise(beds_day_rank, num_quantiles = 5),
    cancer_bin = quantise(cancer_rank, num_quantiles = 5),
    diagnostic_bin = quantise(diagnostic_rank, num_quantiles = 5),
    rtt_bin = quantise(rtt_rank, num_quantiles = 5)
  ) |>
  select(
    `Trust Code`,
    stp_code,
    ends_with("_bin")
  )

bins_sum <-
  bins |>
  rowwise() |>
  mutate(
    bin_sum = sum(
      c_across(!c(`Trust Code`, stp_code)),
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  mutate(
    overall_rank = rank(bin_sum),
    overall_bin = quantise(overall_rank, num_quantiles = 5)
  ) |>
  arrange(desc(overall_rank))

bins_worst_count <-
  bins_sum |>
  mutate(
    sum_of_5s = count_if_worst(ae_bin) +
      count_if_worst(beds_night_bin) +
      count_if_worst(beds_day_bin) +
      count_if_worst(cancer_bin) +
      count_if_worst(diagnostic_bin) +
      count_if_worst(rtt_bin)
  )

trust_performance <-
  bins_worst_count |>
  select(
    `Trust Code`,
    `A&E performance (5 = worst performing)` = ae_bin,
    `Bed night occupancy performance (5 = worst performing)` = beds_night_bin,
    `Bed day occupancy performance (5 = worst performing)` = beds_day_bin,
    `Cancer waiting list performance (5 = worst performing)` = cancer_bin,
    `Diagnostic waiting time performance (5 = worst performing)` = diagnostic_bin,
    `RTT performance (5 = worst performing)` = rtt_bin,
    `Overall performance (5 = worst performing)` = overall_bin,
    `No. of services (out of 6) scoring worst in performance` = sum_of_5s
  )

# ---- Find worst-performing STP's ----
# Filter to the worst performing Trust in each STP. Use rank as the first
# criteria, followed by number of services scoring worst in performance
# (sum_of_5s)
stp_worst <-
  bins_worst_count |>
  group_by(stp_code) |>
  arrange(desc(overall_rank), desc(sum_of_5s)) |>
  slice(1) |>
  ungroup()

stp_performance <-
  stp_worst |>
  select(
    stp_code,
    `A&E performance (5 = worst performing)` = ae_bin,
    `Bed night occupancy performance (5 = worst performing)` = beds_night_bin,
    `Bed day occupancy performance (5 = worst performing)` = beds_day_bin,
    `Cancer waiting list performance (5 = worst performing)` = cancer_bin,
    `Diagnostic waiting time performance (5 = worst performing)` = diagnostic_bin,
    `RTT performance (5 = worst performing)` = rtt_bin,
    `Overall performance (5 = worst performing)` = overall_bin,
    `No. of services (out of 6) scoring worst in performance` = sum_of_5s
  )

# ---- Join boundary data ----
england_stp_performance_boundaries <-
  boundaries_stp |>
  left_join(stp_performance)

use_data(england_stp_performance_boundaries, overwrite = TRUE)