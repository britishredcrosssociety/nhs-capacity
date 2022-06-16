# ---- Load libs ----
library(tidyverse)
library(geographr)
library(sf)

# ---- Load funs ----
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE, ties.method = "max")

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
  left_join(rtt, by = "NHS Board") |>
  mutate(across(starts_with("Percentage"), ~ .x * 100))

# ---- Find worst-performing Trusts across all metrics ----
# Lower rank (e.g., 2 is lower than 1) = lower (worse) rank
ranks <-
  combined |>
  mutate(
    ae_rank = inverse_rank(`Percentage seen within 4 hours`),
    beds_rank = inverse_rank(`Average number of available staffed beds`),
    cancer_rank = rank(`Average number of days waited from receipt of an urgent referral with suspicion of cancer to first cancer treatment (62 day standard)`, ties.method = "max"),
    dtoc_rank = rank(`Bed days occupied by delayed discharge patients`, ties.method = "max"),
    rtt_rank = inverse_rank(`Percentage seen within 18 weeks`)
  ) |>
  select(`NHS Board`, ends_with("_rank"))

ranks_sum <-
  ranks |>
  rowwise() |>
  mutate(
    rank_sum = sum(
      c_across(!`NHS Board`),
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
    `Bed occupancy rank` = beds_rank,
    `Cancer waiting list rank` = cancer_rank,
    `DToC rank` = dtoc_rank,
    `RTT rank` = rtt_rank,
    `Overall rank` = overall_rank
  )

# ---- Join raw (combined) data back to ranks ----
ranks_and_raw <-
  ranks_sum_renamed |>
  left_join(combined)

# ---- Vars ----
ranks_and_raw_renamed <-
  ranks_and_raw |>
  rename(
    `A&E: Percentage seen within 4 hours` = `Percentage seen within 4 hours`,
    `Bed Occupancy: Average number of available staffed beds` = `Average number of available staffed beds`,
    `Delayed Transfer of Care: Bed days occupied by delayed discharge patients` = `Bed days occupied by delayed discharge patients`,
    `Referral to Treatment Wait Times: Percentage seen within 18 weeks` = `Percentage seen within 18 weeks`
  )
# ---- Join boundaries ----
# Match Health Boundary names
ranks_and_raw_matched <-
  ranks_and_raw_renamed |>
  rename(hb19_name = `NHS Board`) |>
  mutate(hb19_name = str_replace_all(hb19_name, "&", "and"))

scotland_performance <-
  boundaries_hb19 |>
  left_join(ranks_and_raw_matched, by = "hb19_name")

scotland_performance |>
  write_rds("preprocess/data/scotland_performance.rds")
