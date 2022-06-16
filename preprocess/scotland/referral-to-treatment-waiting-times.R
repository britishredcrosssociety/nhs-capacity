library(tidyverse)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("preprocess/data/raw/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
scotland_referral_to_treatment_waiting_times <-
  raw |>
  filter(`NHS Board` != "Golden Jubilee Foundation") |>
  group_by(`NHS Board`) |>
  summarise(
    `Percentage seen within 18 weeks` = min(`Percentage seen within 18 weeks`, na.rm = TRUE)
  ) |>
  ungroup()

scotland_referral_to_treatment_waiting_times |>
  write_rds("preprocess/data/scotland_referral_to_treatment_waiting_times.rds")
