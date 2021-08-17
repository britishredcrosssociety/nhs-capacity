library(tidyverse)
library(lubridate)

raw <-
  read_csv(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-cwt-62-day-wait-by-trust-q4-20-21.csv"
  )

northern_ireland_cancer_waiting_lists <-
  raw |>
  rename(Month = `Treatment Month`, Trust = `HSC Trust`) |>
  mutate(Month = dmy(glue::glue("01-{Month}"))) |>
  filter(Month == max(Month)) |>
  select(-Month)

northern_ireland_cancer_waiting_lists |>
write_rds("preprocess/data/northern_ireland_cancer_waiting_lists.rds")