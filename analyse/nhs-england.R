# ---- Libraries ----
library(tidyverse)
library(httr)
library(readxl)
library(janitor)

# ---- A&E Attendance ----
# Source:
# - https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/

# Date: January 2021

GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/January-2021-AE-by-provider-O64J2.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

ae <- read_excel(tf, sheet = "Provider Level Data", skip = 15)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
ae <-
  ae %>%
  slice(-(1:2))

# Remove empty rows at the end of the spreadsheet
ae <-
  ae %>%
  drop_na()

# Keep vars of interest
ae <-
  ae %>%
  select(
    org_code = Code,
    name = Name,
    attendances_type_1 = `Type 1 Departments - Major A&E...4`,
    attendances_type_2 = `Type 2 Departments - Single Specialty...5`,
    attendences_type_3 = `Type 3 Departments - Other A&E/Minor Injury Unit...6`,
    attendences_total = `Total attendances`,
    perc_4_hours_or_less_type_1 = `Percentage in 4 hours or less (type 1)`,
    perc_4_hours_or_less_type_2 = `Percentage in 4 hours or less (type 2)`,
    perc_4_hours_or_less_type_3 = `Percentage in 4 hours or less (type 3)`,
    perc_4_hours_or_less_all = `Percentage in 4 hours or less (all)`,
    num_patients_more_4_hours_from_decision_to_admit_to_admission = `Number of patients spending >4 hours from decision to admit to admission`,
    num_patients_more_12_hours_from_decision_to_admit_to_admission = `Number of patients spending >12 hours from decision to admit to admission`
  )

# Replace '-' character with NA
ae <-
  ae %>%
  mutate(
    across(
      .cols = !c(org_code, name),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
ae <-
  ae %>%
  mutate(
    across(
      .cols = !c(org_code, name),
      as.double
    )
  )

# Save
ae %>%
  write_csv("data/nhs-ae.csv")

# ---- Bed Occupancy ----
# Source:
# - https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/

# Date: December 2020

# Note: In general hospitals will experience capacity pressures at lower overall
# occupancy rates than would previously have been the case.

# - Night -
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

beds_nights <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
beds_nights <-
  beds_nights %>%
  slice(-(1:2))

# Select cols
beds_nights <-
  beds_nights %>%
  select(
    org_code = `Org Code`,
    name = `Org Name`,
    perc_bed_occupied_total = Total...18,
    perc_bed_occupied_general_acute = `General & Acute...19`,
    perc_bed_occupied_learning_disabilities = `Learning Disabilities...20`,
    perc_bed_occupied_maternity = Maternity...21,
    perc_bed_occupied_mental_illness = `Mental Illness...22`,
  )

# Replace '-' character with NA
beds_nights <-
  beds_nights %>%
  mutate(
    across(
      .cols = !c(org_code, name),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
beds_nights <-
  beds_nights %>%
  mutate(
    across(
      .cols = !c(org_code, name),
      as.double
    )
  )

# Save
beds_nights %>%
  write_csv("data/nhs_beds_nights.csv")

# - Day -
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Day-Only-Web_File-Final-DE5WC.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

beds_days <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
beds_days <-
  beds_days %>%
  slice(-(1:2))

# Select cols
beds_days <-
  beds_days %>%
  select(
    org_code = `Org Code`,
    name = `Org Name`,
    perc_bed_occupied_total = Total...18,
    perc_bed_occupied_general_acute = `General & Acute...19`,
    perc_bed_occupied_learning_disabilities = `Learning Disabilities...20`,
    perc_bed_occupied_maternity = Maternity...21,
    perc_bed_occupied_mental_illness = `Mental Illness...22`,
  )

# Replace '-' character with NA
beds_days <-
  beds_days %>%
  mutate(
    across(
      .cols = !c(org_code, name),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
beds_days <-
  beds_days %>%
  mutate(
    across(
      .cols = !c(org_code, name),
      as.double
    )
  )

# Save
beds_days %>%
  write_csv("data/nhs_beds_days.csv")

# ---- Cancer Waiting Times ----
# Source:
# - https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/

# Date: December 2020

GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Cancer-Waiting-Times-Apr-Dec-2020-Data-Extract-Provider.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

cancer_wait_times <- read_excel(tf)

unlink(tf)
rm(tf)

# Make colnames snake_case and drop cols
cancer_wait_times <-
  cancer_wait_times %>%
  clean_names()

# Filter latest month
cancer_wait_times <-
  cancer_wait_times %>%
  filter(month == "DEC")

# Drop cols
cancer_wait_times <-
  cancer_wait_times %>%
  select(
    org_code,
    standard,
    total_treated,
    within_standard,
    breaches
  )

# Summarise
cancer_wait_times <-
  cancer_wait_times %>%
  group_by(
    org_code,
    standard
  ) %>%
  summarise(
    total_treated = sum(total_treated),
    within_standard = sum(within_standard),
    breaches = sum(breaches)
  ) %>%
  ungroup()

# Rename 'standard' 2WW name
cancer_wait_times <-
  cancer_wait_times %>%
  mutate(
    standard = if_else(standard == "2WW", "2 week wait", standard)
  )

# Save
cancer_wait_times %>%
  write_csv("data/nhs_cancer_wait_times.csv")

# ---- Monthly Outpatient Referrals Data ----
# Source:
# - https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-referrals/

# Date: Decemeber 2020

GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/MRR_Prov-Web-file-December-20-EZJ4P.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

monthly_outpatient <- read_excel(tf, sheet = "Provider", skip = 13)

unlink(tf)
rm(tf)

# Make colnames snake_case and drop cols
monthly_outpatient <-
  monthly_outpatient %>%
  clean_names()

# Remove first two entries (one is totals, other is blank)
monthly_outpatient <-
  monthly_outpatient %>%
  slice(-(1:2))

# Sort cols
monthly_outpatient <-
  monthly_outpatient %>%
  select(-c(year:region_name)) %>%
  rename(name = org_name)

# Save
monthly_outpatient %>%
  write_csv("data/nhs_monthly_outpatients_referrals.csv")
