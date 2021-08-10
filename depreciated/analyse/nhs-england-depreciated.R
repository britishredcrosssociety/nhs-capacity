# ---- Load libraries ----
library(tidyverse)
library(sf)
library(httr)
library(readxl)
library(janitor)
library(lubridate)

# ---- A&E Attendance ----
# - Provider -
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/December-2020-AE-by-provider-8c90a.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

eng_ae <- read_excel(tf, sheet = "Provider Level Data", skip = 15)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
eng_ae <-
  eng_ae |>
  slice(-(1:2))

# Remove empty rows at the end of the spreadsheet
eng_ae <-
  eng_ae |>
  drop_na()

# Keep vars of interest
eng_ae <-
  eng_ae |>
  select(
    provider_code = Code,
    total_attendances_more_4_hours = `Total Attendances > 4 hours`,
    perc_4_hours_or_less_type_1 = `Percentage in 4 hours or less (type 1)`,
    perc_4_hours_or_less_all = `Percentage in 4 hours or less (all)`,
    num_patients_more_4_hours_from_decision_to_admit_to_admission = `Number of patients spending >4 hours from decision to admit to admission`,
    num_patients_more_12_hours_from_decision_to_admit_to_admission = `Number of patients spending >12 hours from decision to admit to admission`
  )

# Replace '-' character with NA
eng_ae <-
  eng_ae |>
  mutate(
    across(
      .cols = everything(),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
eng_ae <-
  eng_ae |>
  mutate(
    across(
      .cols = !provider_code,
      as.double
    )
  )

# Save
eng_ae |>
  write_csv("data/processed/nhs_eng_ae_provider.csv")

# - STP -
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/December-2020-AE-by-provider-8c90a.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

eng_ae_stp <- read_excel(tf, sheet = "STP Level Data", skip = 15)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
eng_ae_stp <-
  eng_ae_stp |>
  slice(-(1:2))

# Keep vars of interest
eng_ae_stp <-
  eng_ae_stp |>
  select(
    stp_code = Code,
    total_attendances_more_4_hours = `Total Attendances > 4 hours`,
    perc_4_hours_or_less_type_1 = `Percentage in 4 hours or less (type 1)`,
    perc_4_hours_or_less_all = `Percentage in 4 hours or less (all)`,
    num_patients_more_4_hours_from_decision_to_admit_to_admission = `Number of patients spending >4 hours from decision to admit to admission`,
    num_patients_more_12_hours_from_decision_to_admit_to_admission = `Number of patients spending >12 hours from decision to admit to admission`
  )

# Replace '-' character with NA
eng_ae_stp <-
  eng_ae_stp |>
  mutate(
    across(
      .cols = everything(),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
eng_ae_stp <-
  eng_ae_stp |>
  mutate(
    across(
      .cols = !stp_code,
      as.double
    )
  )

# Save
eng_ae_stp |>
  write_csv("data/processed/nhs_eng_ae_stp.csv")

# # ---- Ambulance Quality Indicators ----
# https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/AmbSYS-December-2020.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

# column names and types to use for loading all data
ambo_colnames <- c("provider_code", "ambulance_service", "count_incidents", "blank", "total_hours", "mean_min_sec", "centile_90th_min_sec")
ambo_types <- c("text", "text", "numeric", "numeric", "numeric", "date", "date")

# Category 1
eng_ambo_cat1 <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C8:I18",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(category = "cat1")

# Category 1T
eng_ambo_cat1t <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C22:I32",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(category = "cat1t")

# Category 2
eng_ambo_cat2 <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C36:I46",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(category = "cat2")

# Category 3
eng_ambo_cat3 <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C50:I60",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(category = "cat3")

# Category 4
eng_ambo_cat4 <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C64:I74",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(category = "cat4")

unlink(tf)
rm(tf)

# combine stats
eng_ambo <- bind_rows(
  eng_ambo_cat1,
  eng_ambo_cat1t,
  eng_ambo_cat2,
  eng_ambo_cat3,
  eng_ambo_cat4
)

# Reformat dates
eng_ambo <-
  eng_ambo |>
  mutate(
    mean_min_sec = format(mean_min_sec, format = "%M:%S"),
    centile_90th_min_sec = format(centile_90th_min_sec, format = "%M:%S")
  )

# Save to raw
eng_ambo |>
  write_csv("data/processed/nhs_eng_ambulance_provider.csv")

# # ---- Bed Occupancy ----
# Source:
# - overnight: https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-overnight/
# - day: https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-day-only/

# - Night -
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

eng_beds_nights <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
eng_beds_nights <-
  eng_beds_nights |>
  slice(-(1:2))

# Select cols
eng_beds_nights <-
  eng_beds_nights |>
  select(org_code = `Org Code`, perc_bed_occupied = Total...18)

# Replace '-' with NA and convert to double
eng_beds_nights <-
  eng_beds_nights |>
  mutate(
    perc_bed_occupied = str_replace_all(perc_bed_occupied, "-", NA_character_),
    perc_bed_occupied = as.double(perc_bed_occupied)
  )

# Save
eng_beds_nights |>
  write_csv("data/processed/nhs_eng_beds_nights_provider.csv")

# - Day -
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Day-Only-Web_File-Final-DE5WC.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

eng_beds_days <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
eng_beds_days <-
  eng_beds_days |>
  slice(-(1:2))

# Select cols
eng_beds_days <-
  eng_beds_days |>
  select(org_code = `Org Code`, perc_bed_occupied = Total...18)

# Replace '-' with NA and convert to double
eng_beds_days <-
  eng_beds_days |>
  mutate(
    perc_bed_occupied = str_replace_all(perc_bed_occupied, "-", NA_character_),
    perc_bed_occupied = as.double(perc_bed_occupied)
  )

# save to raw
eng_beds_days |>
  write_csv("data/processed/nhs_eng_beds_days_provider.csv")

# # ---- DToC ----
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Trust-Type-B-February-2020-4W5PA.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

eng_dtoc <- read_excel(tf, sheet = "Trust - by responsible org", skip = 13)

unlink(tf)
rm(tf)

eng_dtoc <-
  eng_dtoc |>
  slice(-(1:2)) |>
  remove_empty("cols") |>
  select(
    provider_code = Code,
    nhs_dtoc_days = NHS...5,
    social_care_dtoc_days = `Social Care...6`
  )

# Save to raw
eng_dtoc |>
  write_csv("data/processed/nhs_eng_dtoc_provider.csv")

# # ---- Inpatients (elective) & Outpatients ----
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/quarterly-hospital-activity/qar-data/
# Provider based
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/05/QAR-PROV-Web-1920-Q4-aIu8F.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

eng_in_out <- read_excel(tf, sheet = "Full Extract", skip = 16)

unlink(tf)
rm(tf)

eng_in_out <-
  eng_in_out |>
  slice(-(1:2)) |>
  select(
    org_code = `Org Code`,
    inpatient_admissions = Admissions,
    inpatient_failed_to_attend = `Failed to Attend`,
    outpatient_first_attendances_seen = `First Attendances Seen`,
    outpatient_first_attendances_dna = `First Attendances DNA`,
    outpatient_subsequent_attendances_seen = `Subsequent Attendances Seen`,
    outpatient_subsequent_attendances_dna = `Subsequent Attendances DNA`
  )

# Save to raw
eng_in_out |>
  write_csv("data/processed/nhs_eng_in_out_provider.csv")


# To Do:
# - Should RTT waiting list times be included, instead of/in addition to monthly
#   diagnostics data below? Be sure to update README data sets.
#   source: https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/
#   See waiting time analysis already completed: https://github.com/britishredcrosssociety/covid-19-reach/tree/master/analysis/nhs-waiting-times
# - Look through Health Index word doc for missing indicators

# # ---- Monthly Diagnostics ----
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2020-21/
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Monthly-Diagnostics-Web-File-Provider-December-2020_C9B31.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

eng_diagnostics <- read_excel(tf, sheet = "Provider", skip = 13)

unlink(tf)
rm(tf)

eng_diagnostics <-
  eng_diagnostics |>
  select(
    code = `Provider Code`,
    trust_name = `Provider Name`,
    total_waiting_list = `Total Waiting List`,
    perc_waiting_6_plus_weeks = `Percentage waiting 6+ weeks`
  ) |>
  slice(-(1:2))

# Save
eng_diagnostics |>
  write_csv("data/processed/nhs_eng_diagnostics_provider.csv")

# ---- Referral to Treatment Waiting Times (RTT) ----
# Extract data
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Full-CSV-data-file-Dec20-ZIP-2705K-98040.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)
unzip(tf, exdir = tempdir())
unlink(tf)

eng_rtt <- read_csv(list.files(tempdir(), pattern = "*.csv", full.names = TRUE))

# Calculate STP/ICS totals
eng_rtt <-
  eng_rtt |>
  mutate(`Gt 18 Weeks SUM 1` = rowSums(across(`Gt 18 To 19 Weeks SUM 1`:`Gt 52 Weeks SUM 1`), na.rm = TRUE)) |>
  group_by(`Provider Parent Org Code`, `Provider Parent Name`, `Treatment Function Name`) |>
  summarise(
    `Total waiting > 52 weeks` = sum(`Gt 52 Weeks SUM 1`, na.rm = TRUE),
    `Total waiting > 18 weeks` = sum(`Gt 18 Weeks SUM 1`, na.rm = TRUE)
  )

# Save
eng_rtt |>
  write_csv("data/processed/nhs_eng_rtt_provider.csv")


# ---- Care home beds ----
# Source: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
# Care directory with filters
GET(
  "https://www.cqc.org.uk/sites/default/files/HSCA_Active_Locations_1_February_2021.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

eng_care_home <- read_excel(tf, sheet = "HSCA Active Locations", col_types = "text")

unlink(tf)
rm(tf)

# Remove long vars making printing difficult to read
eng_care_home <-
  eng_care_home |>
  select(-contains("(note;"))

# Care home beds LA
eng_care_home_beds_la <-
  eng_care_home |>
  group_by(`Location Local Authority`) |>
  mutate(`Care homes beds` = as.double(`Care homes beds`)) |>
  summarise(`No. care home beds` = sum(`Care homes beds`, na.rm = TRUE)) |>
  rename(lad_name = `Location Local Authority`, care_home_beds = `No. care home beds`)

# Save
eng_care_home_beds_la |>
  write_csv("data/processed/nhs_eng_care_home_beds_la.csv")

# Care home beds without nursing LA
eng_care_home_beds_without_nursing_la <-
  eng_care_home |>
  filter(`Service type - Care home service with nursing` == "Y") |>
  group_by(`Location Local Authority`) |>
  mutate(`Care homes beds` = as.double(`Care homes beds`)) |>
  summarise(`No. care home beds without nursing` = sum(`Care homes beds`, na.rm = TRUE)) |>
  rename(lad_name = `Location Local Authority`, care_home_beds_without_nursing = `No. care home beds without nursing`)

# Save
eng_care_home_beds_without_nursing_la |>
  write_csv("data/processed/nhs_eng_care_home_beds_without_nursing_la.csv")

# Care home beds CCG
eng_care_home_beds_ccg <-
  eng_care_home |>
  group_by(`Location ONSPD CCG Code`) |>
  mutate(`Care homes beds` = as.double(`Care homes beds`)) |>
  summarise(`No. care home beds` = sum(`Care homes beds`, na.rm = TRUE)) |>
  rename(ccg_code = `Location ONSPD CCG Code`, care_home_beds = `No. care home beds`)

# Save
eng_care_home_beds_la |>
  write_csv("data/processed/nhs_eng_care_home_beds_ccg.csv")

# Care home beds without nursing CCG
eng_care_home_beds_without_nursing_ccg <-
  eng_care_home |>
  filter(`Service type - Care home service with nursing` == "Y") |>
  group_by(`Location ONSPD CCG Code`) |>
  mutate(`Care homes beds` = as.double(`Care homes beds`)) |>
  summarise(`No. care home beds without nursing` = sum(`Care homes beds`, na.rm = TRUE)) |>
  rename(ccg_code = `Location ONSPD CCG Code`, care_home_beds_without_nursing = `No. care home beds without nursing`)

# Save
eng_care_home_beds_without_nursing_ccg |>
  write_csv("data/processed/nhs_eng_care_home_beds_without_nursing_ccg.csv")

# Domiciliary care services registered
eng_dom_care_la <-
  eng_care_home |>
  filter(`Service type - Domiciliary care service` == "Y") |>
  count(`Location Local Authority`) |>
  select(lad_name = `Location Local Authority`, num_domiciliary_services = n)

# Save
eng_dom_care_la |>
  write_csv("data/processed/nhs_eng_dom_care_la.csv")

# CCG
eng_dom_care_ccg <-
  eng_care_home |>
  filter(`Service type - Domiciliary care service` == "Y") |>
  count(`Location ONSPD CCG Code`) |>
  select(ccg_name = `Location ONSPD CCG Code`, num_domiciliary_services = n)

# Save
eng_dom_care_ccg |>
  write_csv("data/processed/nhs_eng_dom_care_ccg.csv")
