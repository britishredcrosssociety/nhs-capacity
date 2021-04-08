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

# Ambulance
# Re-read in file as coltype needs specifying
nhs_ambulance <- read_csv(
  "data/nhs_ambulance.csv",
  col_types = cols(
    org_code = col_character(),
    ambulance_service = col_character(),
    count_incidents = col_double(),
    total_hours = col_double(),
    mean_min_sec = col_character(),
    centile_90th_min_sec = col_character(),
    category = col_character()
  )
)

open_trusts %>%
  left_join(
    nhs_ambulance,
    by = "org_code"
  ) %>%
  select(-ambulance_service) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  ) %>%
  select(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    Category = category,
    `Count of Indicents` = count_incidents,
    `Total Response Time (h)` = total_hours,
    `Mean Response Time (min:sec)` = mean_min_sec,
    `90th Centile Response Time (min:sec)` = centile_90th_min_sec
  ) %>%
  mutate(`Total Response Time (h)` = round(`Total Response Time (h)`)) %>%
  write_rds("app/data/ambulance.rds")

# Beds
beds_days <-
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
    `Total Day Beds Occupied` = num_bed_occupied_total,
    `General Acute Day Beds Occupied` = num_bed_occupied_general_acute,
    `Learning Disabilities Day Beds Occupied` = num_bed_occupied_learning_disabilities,
    `Maternity Day Beds Occupied` = num_bed_occupied_maternity,
    `Mental Illness Day Beds Occupied` = num_bed_occupied_mental_illness,
    `% Total Day Beds Occupied` = perc_bed_occupied_total,
    `% General Acute Day Beds Occupied` = perc_bed_occupied_general_acute,
    `% Learning Disabilities Day Beds Occupied` = perc_bed_occupied_learning_disabilities,
    `% Maternity Day Beds Occupied` = perc_bed_occupied_maternity,
    `% Mental Illness Day Beds Occupied` = perc_bed_occupied_mental_illness
  ) %>%
  pivot_longer(
    cols = !starts_with("Trust")
  )

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
  rename(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    `Total Day Beds Occupied` = num_bed_occupied_total,
    `General Acute Day Beds Occupied` = num_bed_occupied_general_acute,
    `Learning Disabilities Day Beds Occupied` = num_bed_occupied_learning_disabilities,
    `Maternity Day Beds Occupied` = num_bed_occupied_maternity,
    `Mental Illness Day Beds Occupied` = num_bed_occupied_mental_illness,
    `% Total Night Beds Occupied` = perc_bed_occupied_total,
    `% General Acute Night Beds Occupied` = perc_bed_occupied_general_acute,
    `% Learning Disabilities Night Beds Occupied` = perc_bed_occupied_learning_disabilities,
    `% Maternity Night Beds Occupied` = perc_bed_occupied_maternity,
    `% Mental Illness Night Beds Occupied` = perc_bed_occupied_mental_illness
  ) %>%
  pivot_longer(
    cols = !starts_with("Trust")
  ) %>%
  bind_rows(beds_days) %>%
  arrange(`Trust Code`) %>%
  write_rds("app/data/beds.rds")

# Cancer wait times
open_trusts %>%
  left_join(
    nhs_cancer_wait_times,
    by = "org_code"
  ) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS"),
    standard = case_when(
      standard == "2WW" ~ "2 Week Wait",
      standard == "2WW Breast" ~ "2 Week Wait Breast",
      TRUE ~ standard
    )
  ) %>%
  mutate(
    across(where(is.double), ~ round(.x, 1))
  ) %>%
  select(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    Standard = standard,
    `Total Treated` = total_treated,
    `Within Standard` = within_standard,
    Breaches = breaches
  ) %>%
  write_rds("app/data/cancer_wait_times.rds")

# Long form
open_trusts %>%
  left_join(
    nhs_cancer_wait_times,
    by = "org_code"
  ) %>%
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS"),
    standard = case_when(
      standard == "2WW" ~ "2 Week Wait",
      standard == "2WW Breast" ~ "2 Week Wait Breast",
      TRUE ~ standard
    )
  ) %>%
  mutate(
    across(where(is.double), ~ round(.x, 1))
  ) %>%
  select(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    Standard = standard,
    `Total Treated` = total_treated,
    `Within Standard` = within_standard,
    Breaches = breaches
  ) %>%
  pivot_longer(cols = where(is.double)) %>%
  write_rds("app/data/cancer_wait_times_long_form.rds")

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
  select(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    `Waiting List Total` = count_total_waiting_list,
    `Waiting 6+ weeks` = count_waiting_6_plus_weeks,
    `Waiting 13+ weeks` = count_waiting_13_plus_weeks
  ) %>%
  pivot_longer(
    cols = !starts_with("Trust")
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
  select(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    `GP Referrals Made (All)` = gp_referrals_made_all,
    `Other Referrals Made (All)` = other_referrals_made_all,
    `GP Referrals Made (Specific Acute)` = gp_referrals_made_specific_acute,
    `Other Referrals Made (Specific Acute)` = other_referrals_made_specific_acute
  ) %>%
  pivot_longer(
    cols = !starts_with("Trust")
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
  mutate(
    across(where(is.double), ~ round(.x, 1))
  ) %>%
  select(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    `Referral Treatment Type` = rtt_type,
    `Waiting 18+ Weeks` = count_wait_more_18_weeks,
    `Waiting 52+ weeks` = count_wait_more_52_weeks,
    `% Waiting 18+ Weeks` = perc_wait_more_18_weeks,
    `% Waiting 52+ Weeks` = perc_wait_more_52_weeks,
    `Total Waiting List` = count_total
  ) %>%
  write_rds("app/data/referral_treatment_waiting_times.rds")

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
  mutate(
    across(where(is.double), ~ round(.x, 1))
  ) %>%
  select(
    `Trust Name` = org_name,
    `Trust Code` = org_code,
    `Referral Treatment Type` = rtt_type,
    `Waiting 18+ Weeks` = count_wait_more_18_weeks,
    `Waiting 52+ weeks` = count_wait_more_52_weeks,
    `% Waiting 18+ Weeks` = perc_wait_more_18_weeks,
    `% Waiting 52+ Weeks` = perc_wait_more_52_weeks,
    `Total Waiting List` = count_total
  ) %>%
  pivot_longer(cols = where(is.double)) %>%
  write_rds("app/data/referral_treatment_waiting_times_long_form.rds")