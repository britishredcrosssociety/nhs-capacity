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
ae <- read_rds("preprocess/data/northern_ireland_ae.rds")
cancer <- read_rds("preprocess/data/northern_ireland_cancer_waiting_lists.rds")
inpatient <- read_rds("preprocess/data/northern_ireland_inpatient_waiting_times.rds")
outpatient <- read_rds("preprocess/data/northern_ireland_outpatient_waiting_times.rds")
reattendance <- read_rds("preprocess/data/northern_ireland_reattendance.rds")

combined <-
  ae |>
  select(-Month) |>
  left_join(cancer, by = "Trust") |>
  left_join(inpatient, by = "Trust") |>
  left_join(outpatient, by = "Trust") |>
  left_join(reattendance, by = "Trust")

# ---- Find worst-performing Trusts across all metrics ----
# Bin each metric into quintiles and look for Trustss in the worst-performing
# quintile across multiple metrics. There are only five Trusts in NI, so can
# just rank them rather than quantise.
bins <-
  combined |>
  mutate(
    ae_bin = inverse_rank(`Percent Under 4 Hours`),
    cancer_bin = inverse_rank(`% treated within 62 days`),
    rtt_in_bin = rank(`Inpatient and day case: % waiting > 52 weeks`),
    rtt_out_bin = rank(`Outpatient: % waiting > 52 weeks`),
    reattend_bin = rank(Reattend)
  ) |>
  select(Trust, ends_with("bin"))

bins_sum <-
  bins |>
  rowwise() |>
  mutate(
    bin_sum = sum(
      c_across(!Trust),
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  mutate(
    overall_bin = rank(bin_sum)
  ) |>
  arrange(desc(overall_bin))

bins_worst_count <-
  bins_sum |>
  mutate(
    sum_of_5s = count_if_worst(ae_bin) +
      count_if_worst(cancer_bin) +
      count_if_worst(rtt_in_bin) +
      count_if_worst(rtt_out_bin) +
      count_if_worst(reattend_bin)
  )

performance <-
  bins_worst_count |>
  select(
    trust_name = Trust,
    `A&E performance (5 = worst performing)` = ae_bin,
    `Cancer waiting list performance (5 = worst performing)` = cancer_bin,
    `Inpatient and day case waiting list performance (5 = worst performing)` = rtt_in_bin,
    `Outpatient waiting list performance (5 = worst performing)` = rtt_out_bin,
    `Reattendance performance (5 = worst performing)` = reattend_bin,
    `Overall performance (5 = worst performing)` = overall_bin,
    `No. of services (out of 5) scoring worst in performance` = sum_of_5s
  ) |>
  mutate(
    trust_name = str_c(trust_name, " Health and Social Care Trust")
  )

# ---- Join boundary data ----
northern_ireland_performance <-
  boundaries_trusts_ni |>
  left_join(performance)

northern_ireland_performance |>
write_rds("preprocess/data/northern_ireland_performance.rds")