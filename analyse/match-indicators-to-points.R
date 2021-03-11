# ---- Libraries ----
library(tidyverse)
library(sf)

# ---- Load data ----
# Load indicators into global env
file_path <- "data/"

file_names <-
  list.files(file_path) %>%
  .[str_detect(., "^nhs_")]

file_names %>%
  map(
    function(file_name) {
      assign(
        x = str_remove(file_name, ".csv"),
        value = read_csv(paste0(file_path, file_name)),
        envir = .GlobalEnv
      )
    }
  )

# Load Trust geometries
points_trusts <-
  read_sf("data/points_nhs_trusts.geojson")

# Keep only open trusts to match to indicators
open_trusts <-
  points_trusts %>%
  as_tibble() %>%
  filter(status == "open") %>%
  select(
    org_name,
    org_code
  )

# ---- Match indicators to open trusts and save to app ----
# AE
open_trusts %>%
  left_join(
    nhs_ae,
    by = "org_code"
  ) %>%
  select(-name) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  rename(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    `No. Type 1 Attendance` = attendances_type_1,
    `No. Type 2 Attendance` = attendances_type_2,
    `No. Type 3 Attendance` = attendences_type_3,
    `No. Attendance Total` = attendences_total,
    `% Type 1 <= 4 hours` = perc_4_hours_or_less_type_1,
    `% Type 2 <= 4 hours` = perc_4_hours_or_less_type_2,
    `% Type 3 <= 4 hours` = perc_4_hours_or_less_type_3,
    `% Total <= 4 hours` = perc_4_hours_or_less_all,
    `No. patients >= 4h from decisiion to admit to admission` = num_patients_more_4_hours_from_decision_to_admit_to_admission,
    `No. patients >= 12h from decisiion to admit to admission` = num_patients_more_12_hours_from_decision_to_admit_to_admission
  ) %>%
  pivot_longer(
    cols = !starts_with("Trust")
  ) %>%
  write_rds("app/data/ae.rds")

# Beds Days
open_trusts %>%
  left_join(
    nhs_beds_days,
    by = "org_code"
  ) %>%
  select(-name) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  rename(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    `% Total Day Beds Occupied` = perc_bed_occupied_total,
    `% General Acute Day Beds Occupied` = perc_bed_occupied_general_acute,
    `% Learning Disabilities Day Beds Occupied` = perc_bed_occupied_learning_disabilities,
    `% Maternity Day Beds Occupied` = perc_bed_occupied_maternity,
    `% Mental Illness Day Beds Occupied` = perc_bed_occupied_mental_illness
  ) %>%
  pivot_longer(
    cols = !starts_with("Trust")
  ) %>%
  write_rds("app/data/beds_days.rds")

# Beds Nights
open_trusts %>%
  left_join(
    nhs_beds_nights,
    by = "org_code"
  ) %>%
  select(-name) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  write_rds("app/data/beds_nights.rds")

# Cancer wait times
open_trusts %>%
  left_join(
    nhs_cancer_wait_times,
    by = "org_code"
  ) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  write_rds("app/data/cancer_wait_times.rds")

# Diagnostic wait times
open_trusts %>%
  left_join(
    nhs_diagnostic_waiting_times,
    by = "org_code"
  ) %>%
  select(-name) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  write_rds("app/data/diagnostic_wait_times.rds")

# Monthly outpatient referrals
open_trusts %>%
  left_join(
    nhs_outpatients_referrals,
    by = "org_code"
  ) %>%
  select(-name) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  write_rds("app/data/outpatients_referrals.rds")

# Referral Treatment Waiting Times
open_trusts %>%
  left_join(
    nhs_referral_treatment_waiting_times,
    by = "org_code"
  ) %>%
  select(-name) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  write_rds("app/data/referral_treatment_waiting_times.rds")
