# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(janitor)
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
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/AmbSYS-Jun21.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

# column names and types to use for loading all data
ambo_colnames <-
  c(
    "Trust Code",
    "Ambulance Service",
    "Incident Count",
    "blank",
    "Total Response Time (h)",
    "Mean Response Time (min:sec)",
    "90th Centile Response Time (min:sec)"
  )

ambo_types <-
  c(
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "date",
    "date"
  )

# Category 1
eng_ambo_cat1 <-
  read_excel(
    tf,
    sheet = "Response times",
    range = "C8:I18",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(Category = "cat1")

# Category 1T
eng_ambo_cat1t <-
  read_excel(
    tf,
    sheet = "Response times",
    range = "C22:I32",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(Category = "cat1t")

# Category 2
eng_ambo_cat2 <-
  read_excel(
    tf,
    sheet = "Response times",
    range = "C36:I46",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(Category = "cat2")

# Category 3
eng_ambo_cat3 <-
  read_excel(
    tf,
    sheet = "Response times",
    range = "C50:I60",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(Category = "cat3")

# Category 4
eng_ambo_cat4 <-
  read_excel(
    tf,
    sheet = "Response times",
    range = "C64:I74",
    col_names = ambo_colnames,
    col_types = ambo_types
  ) |>
  remove_empty("cols") |>
  mutate(Category = "cat4")

# combine stats
eng_ambo_raw <-
  bind_rows(
    eng_ambo_cat1,
    eng_ambo_cat1t,
    eng_ambo_cat2,
    eng_ambo_cat3,
    eng_ambo_cat4
  )

# Reformat dates
eng_ambo_dates <-
  eng_ambo_raw |>
  mutate(
    `Mean Response Time (min:sec)` = format(`Mean Response Time (min:sec)`, format = "%M:%S"),
    `90th Centile Response Time (min:sec)` = format(`90th Centile Response Time (min:sec)`, format = "%M:%S"),
    `Total Response Time (h)` = round(`Total Response Time (h)`)
  )

# Filter to only open trusts
england_ambulance_quality_indicators <-
  open_trusts |>
  left_join(
    eng_ambo_dates,
    by = "Trust Code"
  )

# Save
england_ambulance_quality_indicators |>
write_rds("preprocess/data/england_ambulance_quality_indicators.rds")