# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(geographr)

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
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/Monthly-Diagnostics-Web-File-Provider-May-2021_84CSC.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

raw <-
  read_excel(
    tf,
    sheet = "Provider",
    skip = 13
  )

# Remove first two rows (summary & blank)
diagnostics_sliced <-
  raw |>
  slice(-(1:2))

# Select cols
diagnostics_vars <-
  diagnostics_sliced |>
  select(
    `Trust Code` = `Provider Code`,
    `Waiting List Total` = `Total Waiting List`,
    `Waiting 6+ weeks` = `Number waiting 6+ Weeks`,
    `Waiting 13+ weeks` = `Number waiting 13+ Weeks`
  )

# Calculate relative scores
diagnostics_scores <-
  diagnostics_vars |>
  mutate(
    `% waiting 6+ weeks` = `Waiting 6+ weeks` / `Waiting List Total`,
    `% waiting 13+ weeks` = `Waiting 13+ weeks` / `Waiting List Total`
  )

# Filter to only open trusts
england_diagnostic_wait_times <-
  open_trusts |>
  left_join(
    diagnostics_scores,
    by = "Trust Code"
  )

# # Pivot longer
# england_diagnostic_wait_times_longer <-
#   england_diagnostic_wait_times |>
#   pivot_longer(
#     cols = !starts_with("Trust")
#   )

# Save
england_diagnostic_wait_times |>
write_rds("preprocess/data/england_diagnostic_wait_times.rds")