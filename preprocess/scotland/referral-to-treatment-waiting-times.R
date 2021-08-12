library(tidyverse)
library(usethis)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("depreciated/data/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
referral_to_treatment_waiting_times <-
  raw |>
  filter(`NHS Board` != "National") |>
  group_by(`NHS Board`) |>
  summarise(
    `Percentage seen within 18 weeks` = min(`Percentage seen within 18 weeks`, na.rm = TRUE)
  ) |>
  ungroup()

use_data(referral_to_treatment_waiting_times, overwrite = TRUE)