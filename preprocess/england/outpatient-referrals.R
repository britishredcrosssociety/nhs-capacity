# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(janitor)
library(sf)
library(geographr)
library(usethis)

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    `Trust Code` = nhs_trust_code,
    `Trust Name` = nhs_trust_name
  ) |>
  mutate(
    `Trust Name` = str_to_title(`Trust Name`),
    `Trust Name` = str_replace(`Trust Name`, "Nhs", "NHS")
  )

# Load raw data
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/MRR_Prov-Web-file-May-21-0P6EL.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

raw <-
  read_excel(
    tf,
    sheet = "Provider",
    skip = 13
  )

# Make colnames snake_case and drop cols
outpatient_snake_case <-
  raw |>
  clean_names()

# Remove first two entries (one is totals, other is blank)
outpatient_sliced <-
  outpatient_snake_case |>
  slice(-(1:2))

# Sort cols
outpatient_vars <-
  outpatient_sliced |>
  select(
    `Trust Code` = org_code,
    `GP Referrals Made (All)` = gp_referrals_made_all,
    `Other Referrals Made (All)` = other_referrals_made_all,
    `GP Referrals Made (Specific Acute)` = gp_referrals_made_specific_acute,
    `Other Referrals Made (Specific Acute)` = other_referrals_made_specific_acute
  )

# Filter to only open trusts
outpatient_open <-
  open_trusts |>
  left_join(
    outpatient_vars,
    by = "Trust Code"
  )

# Pivot longer
outpatient_referrals <-
  outpatient_open |>
  pivot_longer(
    cols = !starts_with("Trust")
  )

# Save
use_data(outpatient_referrals, overwrite = TRUE)