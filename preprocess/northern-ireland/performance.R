# ---- Load libs ----
library(tidyverse)
library(usethis)

# ---- Load funs ----
# Make a higher value rank lowest (lowest = 1)
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

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

bin_sum <-
  bins |>
  rowwise() |>
  mutate(
    bin_sum = sum(
      c_across(!Trust),
      na.rm = TRUE
    )
  ) |>
  ungroup()

bins_ranked <-
  bin_sum |>
  mutate(
    bin_rank = rank(bin_sum)
  ) |>
  arrange(desc(bin_rank))

northern_ireland_performance <-
  bins_ranked |>
  rename(
    `Reattendance performance (5 = worst performing)` = reattend_bin,
    `A&E performance (5 = worst performing)` = ae_bin,
    `Cancer waiting list performance (5 = worst performing)` = cancer_bin,
    `Inpatient and day case waiting list performance (5 = worst performing)` = rtt_in_bin,
    `Outpatient waiting list performance (5 = worst performing)` = rtt_out_bin,
    `Overall rank (5 = worst performing)` = bin_rank
  ) |>
  select(-bin_sum)

use_data(northern_ireland_performance, overwrite = TRUE)