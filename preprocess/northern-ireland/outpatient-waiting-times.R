library(tidyverse)
library(lubridate)

raw <-
  read_csv(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-outpatients-q2-21-22.csv",
    col_types = cols(
      .default = col_character(),
      `Quarter Ending` = col_character(),
      `HSC Trust` = col_character(),
      Specialty = col_character(),
      `Programme Of Care` = col_character()
    )
  )

# Remove commas from the data columns
outpatient_commas <-
  raw |>
  rename(Trust = `HSC Trust`) |>
  mutate(
    across(
      `0 - 6 weeks`:`Total Waiting`,
      ~ as.numeric(str_remove(.x, ","))
    )
  )

outpatient_date <-
  outpatient_commas |>
  mutate(Date = dmy(`Quarter Ending`)) |>
  relocate(Date) |>
  filter(Date == max(Date, na.rm = TRUE))

outpatient_summaries <-
  outpatient_date |>
  group_by(Date, Trust, Specialty) |>
  summarise(
    `Total waiting > 18 weeks` = sum(`>18-52 weeks`, na.rm = TRUE) + sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting > 52 weeks` = sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting` = sum(`Total Waiting`, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(Date, Trust) |>
  summarise(
    `Outpatient: Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE),
    `Outpatient: Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
    `Outpatient: Total waiting` = sum(`Total waiting`, na.rm = TRUE)
  ) |>
  ungroup()

northern_ireland_outpatient_waiting_times <-
  outpatient_summaries |>
  mutate(
    `Outpatient: % waiting > 18 weeks` = `Outpatient: Total waiting > 18 weeks` / `Outpatient: Total waiting`,
    `Outpatient: % waiting > 52 weeks` = `Outpatient: Total waiting > 52 weeks` / `Outpatient: Total waiting`
  ) |>
  select(-Date) |>
  filter(Trust != "Day Case Procedure Centre")

northern_ireland_outpatient_waiting_times |>
  write_rds("preprocess/data/northern_ireland_outpatient_waiting_times.rds")