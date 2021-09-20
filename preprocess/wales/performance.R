# ---- Load libs ----
library(tidyverse)
library(geographr)
library(sf)

# ---- Load funs ----
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE, ties.method = "max")

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
  ) |>
  select(
    lhb_name = `Health Board`,
    `Percentage breached`,
    `Red calls - % of emergency responses arriving at the scene within 8 minutes`,
    `% general and acute beds occupied`,
    `% starting treatment within 62 days`,
    `% waiting 53+ weeks`
  ) |>
  mutate(
    across(
      c(`% general and acute beds occupied`, `% waiting 53+ weeks`),
      ~ .x * 100
    )
  )

# ---- Find worst-performing Trusts across all metrics ----
# Lower rank (e.g., 2 is lower than 1) = lower (worse) rank
ranks <-
  combined |>
  mutate(
    ae_rank = rank(`Percentage breached`, ties.method = "max"),
    ambulance_rank = inverse_rank(`Red calls - % of emergency responses arriving at the scene within 8 minutes`),
    beds_rank = rank(`% general and acute beds occupied`, ties.method = "max"),
    cancer_rank = inverse_rank(`% starting treatment within 62 days`),
    rtt_rank = rank(`% waiting 53+ weeks`, ties.method = "max")
  ) |>
  select(lhb_name, ends_with("_rank"))

ranks_sum <-
  ranks |>
  rowwise() |>
  mutate(
    rank_sum = sum(
      c_across(!lhb_name),
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  mutate(
    overall_rank = rank(rank_sum, ties.method = "max")
  ) |>
  arrange(desc(overall_rank)) |>
  select(-rank_sum)

ranks_sum_renamed <-
  ranks_sum |>
  rename(
    `A&E rank` = ae_rank,
    `Ambulance rank` = ambulance_rank,
    `Bed occupancy rank` = beds_rank,
    `Cancer waiting list rank` = cancer_rank,
    `RTT rank` = rtt_rank,
    `Overall rank` = overall_rank
  )

# ---- Join raw (combined) data back to ranks ----
ranks_and_raw <-
  ranks_sum_renamed |>
  left_join(combined)

# ---- Rename vars ----
ranks_and_raw_renamed <-
  ranks_and_raw |>
  rename(
    `A&E: Percentage breached (4hr)` = `Percentage breached`,
    `Ambulance: Red calls - % of emergency responses arriving at the scene within 8 minutes` = `Red calls - % of emergency responses arriving at the scene within 8 minutes`,
    `Bed Occupancy: % general and acute beds occupied` = `% general and acute beds occupied`,
    `Cancer Wait Times: % starting treatment within 62 days` = `% starting treatment within 62 days`,
    `Referral to Treatment Wait Times: % waiting 53+ weeks` = `% waiting 53+ weeks`
  )

# ---- Join boundary data ----
# Match Health Boundary names
ranks_and_raw_matched <-
  ranks_and_raw_renamed |>
  mutate(
    lhb_name = str_replace_all(
      lhb_name,
      "Local Health Board",
      "Health Board"
    )
  )

wales_performance <-
  boundaries_lhb |>
  left_join(ranks_and_raw_matched)

wales_performance |>
  write_rds("preprocess/data/wales_performance.rds")