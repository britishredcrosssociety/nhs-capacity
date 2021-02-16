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
  eng_ae %>%
  slice(-(1:2))

# Remove empty rows at the end of the spreadsheet
eng_ae <-
  eng_ae %>%
  drop_na()

# Keep vars of interest
eng_ae <-
  eng_ae %>%
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
  eng_ae %>%
  mutate(
    across(
      .cols = everything(),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
eng_ae <-
  eng_ae %>%
  mutate(
    across(
      .cols = !provider_code,
      as.double
    )
  )

# Save
eng_ae %>%
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
  eng_ae_stp %>%
  slice(-(1:2))

# Keep vars of interest
eng_ae_stp <-
  eng_ae_stp %>%
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
  eng_ae_stp %>%
  mutate(
    across(
      .cols = everything(),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
eng_ae_stp <-
  eng_ae_stp %>%
  mutate(
    across(
      .cols = !stp_code,
      as.double
    )
  )

# Save
eng_ae_stp %>%
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
  ) %>%
  remove_empty("cols") %>%
  mutate(category = "cat1")

# Category 1T
eng_ambo_cat1t <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C22:I32",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) %>%
  remove_empty("cols") %>%
  mutate(category = "cat1t")

# Category 2
eng_ambo_cat2 <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C36:I46",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) %>%
  remove_empty("cols") %>%
  mutate(category = "cat2")

# Category 3
eng_ambo_cat3 <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C50:I60",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) %>%
  remove_empty("cols") %>%
  mutate(category = "cat3")

# Category 4
eng_ambo_cat4 <-
  read_excel(
    tf,
    sheet = "Response Times",
    range = "C64:I74",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) %>%
  remove_empty("cols") %>%
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
  eng_ambo %>%
  mutate(
    mean_min_sec = format(mean_min_sec, format = "%M:%S"),
    centile_90th_min_sec = format(centile_90th_min_sec, format = "%M:%S")
  )

# Save to raw
eng_ambo %>%
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
  eng_beds_nights %>%
  slice(-(1:2))

# Select cols
eng_beds_nights <-
  eng_beds_nights %>%
  select(org_code = `Org Code`, perc_bed_occupied = Total...18)

# Replace '-' with NA and convert to double
eng_beds_nights <-
  eng_beds_nights %>% 
  mutate(
    perc_bed_occupied = str_replace_all(perc_bed_occupied, "-", NA_character_),
    perc_bed_occupied = as.double(perc_bed_occupied)
  )

# Save
eng_beds_nights %>%
  write_csv("data/processed/nhs_eng_beds_nights.csv")

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
  eng_beds_days %>%
  slice(-(1:2))

# Select cols
eng_beds_days <-
  eng_beds_days %>%
  select(org_code = `Org Code`, perc_bed_occupied = Total...18)

# Replace '-' with NA and convert to double
eng_beds_days <-
  eng_beds_days %>% 
  mutate(
    perc_bed_occupied = str_replace_all(perc_bed_occupied, "-", NA_character_),
    perc_bed_occupied = as.double(perc_bed_occupied)
  )

# save to raw
eng_beds_days %>%
  write_csv("data/processed/nhs_eng_beds_days.csv")

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
  eng_dtoc %>%
  slice(-(1:2)) %>%
  remove_empty("cols") %>%
  select(
    provider_code = Code,
    nhs_dtoc_days = NHS...5,
    social_care_dtoc_days = `Social Care...6`
  )

# Save to raw
eng_dtoc %>%
  write_csv("data/processed/nhs_eng_dtoc_provider.csv")
#
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
  eng_in_out %>%
  slice(-(1:2)) %>%
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
eng_in_out %>%
  write_csv("data/raw/nhs_eng_in_out.csv")
#
# # ---- Monthly Diagnostics ----
# # Source: https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2020-21/
# # Helper function for downloading and processing data
# get_waiting_list <-
#   function(url, date) {
#   GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
#
#   read_excel(tf, sheet = "Provider", skip = 13) %>%
#     select(code = `Provider Code`, trust_name = `Provider Name`, total_waiting_list = `Total Waiting List`) %>%
#     slice(-(1:2)) %>%
#     mutate(Date = dmy(date))
# }
#
# # Download waiting list stats for 2020
# eng_diagnostics_dec_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Monthly-Diagnostics-Web-File-Provider-December-2020_C9B31.xls", "01/12/2020")
# eng_diagnostics_nov_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/Monthly-Diagnostics-Web-File-Provider-November-2020_P6PN01.xls", "01/11/2020")
# eng_diagnostics_oct_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/12/Monthly-Diagnostics-Web-File-Provider-October-2020_6CS21.xls", "01/10/2020")
# eng_diagnostics_sep_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Monthly-Diagnostics-Web-File-Provider-September-2020_1ME27.xls", "01/09/2020")
# eng_diagnostics_aug_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/10/Monthly-Diagnostics-Web-File-Provider-August-2020_o1lg9.xls", "01/08/2020")
# eng_diagnostics_jul_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Monthly-Diagnostics-Web-File-Provider-July-2020.xls", "01/07/2020")
# eng_diagnostics_jun_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/08/Monthly-Diagnostics-Web-File-Provider-June-2020.xls", "01/06/2020")
# eng_diagnostics_may_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/07/Monthly-Diagnostics-Web-File-Provider-May-2020.xls", "01/05/2020")
# eng_diagnostics_apr_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/06/Monthly-Diagnostics-Web-File-Provider-April-2020.xls", "01/04/2020")
#
# eng_diagnostics <-
#   bind_rows(
#     eng_diagnostics_dec_20,
#     eng_diagnostics_nov_20,
#     eng_diagnostics_oct_20,
#     eng_diagnostics_sep_20,
#     eng_diagnostics_aug_20,
#     eng_diagnostics_jul_20,
#     eng_diagnostics_jun_20,
#     eng_diagnostics_may_20,
#     eng_diagnostics_apr_20
#   )
#
# # Save to raw
# eng_diagnostics %>%
#   write_csv("data/raw/nhs_eng_diagnostics.csv")
#
# # ---- Care home beds ----
# # Source: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
# # Care directory with filters
# GET("https://www.cqc.org.uk/sites/default/files/HSCA_Active_Locations_1_February_2021.xlsx",
#     write_disk(tf <- tempfile(fileext = ".xlsx")))
#
# cqc_filter <- read_excel(tf, sheet = "HSCA Active Locations", col_types = "text")
#
# unlink(tf)
# rm(tf)
#
# # Care home beds with nursing (per 1,000 older people)
# nursing_beds <-
#   cqc_filter %>%
#   group_by(`Location Local Authority`) %>%
#   mutate(`Care homes beds` = as.double(`Care homes beds`)) %>%
#   summarise(`No. care home beds` = sum(`Care homes beds`, na.rm = TRUE)) %>%
#   left_join(geog_names, by = c("Location Local Authority" = "Name")) %>%
#   left_join(la_pop, by = "Code") %>%
#   mutate(`Care home beds per 1,000 people aged 65+` = `No. care home beds` / `No. people aged 65+` * 1000) %>%
#   select(Code, Name = `Location Local Authority`, everything())
#
# # Domiciliary care services registered
# dom_care <-
#   cqc_filter %>%
#   filter(`Service type - Domiciliary care service` == "Y") %>%
#   count(`Location Local Authority`) %>%
#   left_join(geog_names, by = c("Location Local Authority" = "Name")) %>%
#   select(Code, Name = `Location Local Authority`, `No. domiciliary services` = n)
