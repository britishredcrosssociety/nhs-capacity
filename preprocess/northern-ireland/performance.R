# ---- Load libs ----
library(tidyverse)
library(geographr)
library(sf)

# ---- Load funs ----
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE, ties.method = "max")

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
  left_join(reattendance, by = "Trust") |>
  select(
    Trust,
    `Percent Under 4 Hours`,
    `% treated within 62 days`,
    `Inpatient and day case: % waiting > 52 weeks`,
    `Outpatient: % waiting > 52 weeks`,
    Reattend
  ) |>
  mutate(across(ends_with("> 52 weeks"), ~ .x * 100))

# ---- Find worst-performing Trusts across all metrics ----
# Lower rank (e.g., 2 is lower than 1) = lower (worse) rank
ranks <-
  combined |>
  mutate(
    ae_rank = inverse_rank(`Percent Under 4 Hours`),
    cancer_rank = inverse_rank(`% treated within 62 days`),
    rtt_in_rank = rank(`Inpatient and day case: % waiting > 52 weeks`, ties.method = "max"),
    rtt_out_rank = rank(`Outpatient: % waiting > 52 weeks`, ties.method = "max"),
    reattend_rank = rank(Reattend, ties.method = "max")
  ) |>
  select(Trust, ends_with("_rank"))

ranks_sum <-
  ranks |>
  rowwise() |>
  mutate(
    rank_sum = sum(
      c_across(!Trust),
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
    `Cancer waiting list rank` = cancer_rank,
    `RTT inpatient rank` = rtt_in_rank,
    `RTT outpatient rank` = rtt_out_rank,
    `Reattendance rank` = reattend_rank,
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
    `A&E: % Under 4 Hours` = `Percent Under 4 Hours`,
    `Cancer Wait Times: % treated within 62 days` = `% treated within 62 days`,
    `Referral to Treatment Inpatient & day case: % waiting > 52 weeks` = `Inpatient and day case: % waiting > 52 weeks`,
    `Referral Outpatient: % waiting > 52 weeks` = `Outpatient: % waiting > 52 weeks`,
    `Emergency Care: Reattendance within 7 days: %` = Reattend
  )

# ---- Join boundary data ----
# Make Trust names match
ranks_and_raw_matched <-
  ranks_and_raw_renamed |>
  mutate(
    Trust = str_c(Trust, " Health and Social Care Trust")
  ) |>
  rename(trust_name = Trust)

northern_ireland_performance <-
  boundaries_trusts_ni |>
  left_join(ranks_and_raw_matched)

northern_ireland_performance |>
  write_rds("preprocess/data/northern_ireland_performance.rds")