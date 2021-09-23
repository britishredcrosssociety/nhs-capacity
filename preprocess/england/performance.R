# ---- Load libs ----
library(tidyverse)
library(geographr)
library(sf)

# ---- Load funs ----
# Inverse rank - override imported to make NA's last
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = "keep", ties.method = "max")

# ---- Load data ----
ae <- read_rds("preprocess/data/england_ae.rds")
ambulance <- read_rds("preprocess/data/england_ambulance_quality_indicators.rds")
beds <- read_rds("preprocess/data/england_bed_occupancy.rds")
cancer <- read_rds("preprocess/data/england_cancer_wait_times.rds")
diagnostic <- read_rds("preprocess/data/england_diagnostic_wait_times.rds")
iapt <- read_rds("preprocess/data/england_iapt.rds")
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
  left_join(rtt_processed) |>
  mutate(across(starts_with("%"), ~ .x * 100))

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
# Lower rank (e.g., 2 is lower than 1) = lower (worse) rank
ranks <-
  complete_data |>
  mutate(
    ae_rank = inverse_rank(`% Total <= 4 hours`),
    beds_night_rank = rank(`% Total Night Beds Occupied`, na.last = "keep", ties.method = "max"),
    beds_day_rank = rank(`% Total Day Beds Occupied`, na.last = "keep", ties.method = "max"),
    cancer_rank = rank(`% Breaches`, na.last = "keep", ties.method = "max"),
    diagnostic_rank = rank(`% waiting 13+ weeks`, na.last = "keep", ties.method = "max"),
    rtt_rank = rank(`% Waiting 52+ Weeks`, na.last = "keep", ties.method = "max")
  ) |>
  select(
    `Trust Code`,
    stp_code,
    ends_with("_rank")
  )

ranks_sum <-
  ranks |>
  rowwise() |>
  mutate(
    rank_sum = sum(
      c_across(!c(`Trust Code`, stp_code)),
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  mutate(
    overall_rank = rank(rank_sum, ties.method = "max")
  ) |>
  arrange(desc(overall_rank)) |>
  select(-rank_sum)

# ---- Find worst-performing STP's ----
# Filter to worst performing Trust in each STP.
stp_worst <-
  ranks_sum |>
  group_by(stp_code) |>
  arrange(desc(overall_rank)) |>
  slice(1) |>
  ungroup() |>
  arrange(desc(overall_rank))

# ---- Rank STP's ----
stp_rank <-
  stp_worst |>
  mutate(
    across(
      !c(`Trust Code`, stp_code),
      ~ rank(.x, ties.method = "max")
    )
  )

stp_rank_renamed <-
  stp_rank |>
  rename(
    `A&E rank` = ae_rank,
    `Bed night occupancy rank` = beds_night_rank,
    `Bed day occupancy rank` = beds_day_rank,
    `Cancer waiting list rank` = cancer_rank,
    `Diagnostic waiting time rank` = diagnostic_rank,
    `RTT rank` = rtt_rank,
    `Overall rank` = overall_rank
  )

# ---- Join raw (complete) data back to ranks ----
ranks_and_raw <-
  stp_rank_renamed |>
  left_join(complete_data) |>
  select(-`Trust Code`)

# ---- Join on IAPT data which already at the STP level ----
iapt_processed <-
  iapt |>
  left_join(stp_codes) |>
  select(-stp_code_short) |>
  mutate(`IAPT rank` = inverse_rank(`Referrals that finished a course of treatment in the month waiting 126 days or less for first treatment`))

ranks_and_raw_and_iapt <-
  ranks_and_raw |>
  left_join(iapt_processed) |>
  relocate(`IAPT rank`, .after = `RTT rank`)

# ---- Rename vars ----
ranks_and_raw_renamed <-
  ranks_and_raw_and_iapt |>
  rename(
    `A&E: % Total <= 4 hours` = `% Total <= 4 hours`,
    `Bed Occupancy: % Total Night Beds Occupied` = `% Total Night Beds Occupied`,
    `Bed Occupancy: % Total Day Beds Occupied` = `% Total Day Beds Occupied`,
    `Cancer Wait Times: % Breaches` = `% Breaches`,
    `Diagnostic Wait Times: % waiting 13+ weeks` = `% waiting 13+ weeks`,
    `Referral to Treatment Wait Times:% Waiting 52+ Weeks` = `% Waiting 52+ Weeks`,
    `IAPT: % Finished Course in the Month Waiting 126 Days or Less for First Treatment ` = `Referrals that finished a course of treatment in the month waiting 126 days or less for first treatment`
  )

# ---- Join boundary data ----
england_performance <-
  boundaries_stp |>
  left_join(ranks_and_raw_renamed)

england_performance |>
  write_rds("preprocess/data/england_performance.rds")