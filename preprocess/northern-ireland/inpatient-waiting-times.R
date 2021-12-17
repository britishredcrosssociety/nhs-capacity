library(tidyverse)
library(lubridate)

raw <-
  read_csv(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-total-waiting-q2-21-22.csv",
    col_types = cols(
      .default = col_double(),
      `Quarter Ending` = col_character(),
      `HSCTrust` = col_character(),
      Specialty = col_character(),
      `Programme of Care` = col_character()
    )
  )

inpatient_max_date <-
  raw |>
  rename(Trust = `HSCTrust`) |>
  mutate(Date = dmy(`Quarter Ending`)) |>
  filter(Date == max(Date, na.rm = TRUE))

inpatient_summaries <-
  inpatient_max_date |>
  group_by(Date, Trust, Specialty) |>
  summarise(
    `Total waiting > 52 weeks` = sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting` = sum(`Total`, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(Date, Trust) |>
  summarise(
    `Inpatient and day case: Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
    `Inpatient and day case: Total waiting` = sum(`Total waiting`, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(`Inpatient and day case: % waiting > 52 weeks` = `Inpatient and day case: Total waiting > 52 weeks` / `Inpatient and day case: Total waiting`)

northern_ireland_inpatient_waiting_times <-
  inpatient_summaries |>
  select(-Date) |>
  filter(Trust != "DPC")

northern_ireland_inpatient_waiting_times |>
  write_rds("preprocess/data/northern_ireland_inpatient_waiting_times.rds")