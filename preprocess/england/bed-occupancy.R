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

# ---- Day ----
# Load raw data
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Day-Only-Web_File-Q2-2021-22-Final-XCVFG.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

raw_day <-
  read_excel(
    tf,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_day_sliced <-
  raw_day |>
  slice(-(1:2))

# Select cols
beds_day_vars <-
  beds_day_sliced |>
  select(
    `Trust Code` = `Org Code`,
    `Total Day Beds Occupied` = Total...12,
    `General Acute Day Beds Occupied` = `General & Acute...13`,
    `Learning Disabilities Day Beds Occupied` = `Learning Disabilities...14`,
    `Maternity Day Beds Occupied` = Maternity...15,
    `Mental Illness Day Beds Occupied` = `Mental Illness...16`,
    `% Total Day Beds Occupied` = Total...18,
    `% General Acute Day Beds Occupied` = `General & Acute...19`,
    `% Learning Disabilities Day Beds Occupied` = `Learning Disabilities...20`,
    `% Maternity Day Beds Occupied` = Maternity...21,
    `% Mental Illness Day Beds Occupied` = `Mental Illness...22`,
  )

# Replace '-' character with NA
beds_day_replaced <-
  beds_day_vars |>
  mutate(
    across(
      .cols = !c(`Trust Code`),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
beds_day_double <-
  beds_day_replaced |>
  mutate(
    across(
      .cols = !c(`Trust Code`),
      as.double
    )
  )

# Filter to only open trusts
beds_day <-
  open_trusts |>
  left_join(
    beds_day_double,
    by = "Trust Code"
  )

# # Pivot longer
# beds_day_longer <-
#   beds_day |>
#   pivot_longer(
#     cols = !starts_with("Trust")
#   )

# ---- Night ----
# Load raw data
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q2-2021-22-Final-XCVFG.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

raw_night <-
  read_excel(
    tf,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_night_sliced <-
  raw_night |>
  slice(-(1:2))

# Select cols
beds_night_vars <-
  beds_night_sliced |>
  select(
    `Trust Code` = `Org Code`,
    `Total Night Beds Occupied` = Total...12,
    `General Acute Night Beds Occupied` = `General & Acute...13`,
    `Learning Disabilities Night Beds Occupied` = `Learning Disabilities...14`,
    `Maternity Night Beds Occupied` = Maternity...15,
    `Mental Illness Night Beds Occupied` = `Mental Illness...16`,
    `% Total Night Beds Occupied` = Total...18,
    `% General Acute Night Beds Occupied` = `General & Acute...19`,
    `% Learning Disabilities Night Beds Occupied` = `Learning Disabilities...20`,
    `% Maternity Night Beds Occupied` = Maternity...21,
    `% Mental Illness Night Beds Occupied` = `Mental Illness...22`,
  )

# Replace '-' character with NA
beds_night_replaced <-
  beds_night_vars |>
  mutate(
    across(
      .cols = !c(`Trust Code`),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
beds_night_double <-
  beds_night_replaced |>
  mutate(
    across(
      .cols = !c(`Trust Code`),
      as.double
    )
  )

# Filter to only open trusts
beds_night <-
  open_trusts |>
  left_join(
    beds_night_double,
    by = "Trust Code"
  ) |>
  select(-`Trust Name`)

# # Pivot longer
# beds_night_longer <-
#   beds_night |>
#   pivot_longer(
#     cols = !starts_with("Trust")
#   )

# ---- Join ----
england_bed_occupancy <-
  beds_day |>
  left_join(beds_night, by = "Trust Code")

# ---- Save ----
england_bed_occupancy |>
  write_rds("preprocess/data/england_bed_occupancy.rds")