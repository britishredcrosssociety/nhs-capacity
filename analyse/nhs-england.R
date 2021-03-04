# ---- Libraries ----
library(tidyverse)
library(httr)
library(readxl)

# ---- A&E Attendance ----
# - Provider -
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/
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
