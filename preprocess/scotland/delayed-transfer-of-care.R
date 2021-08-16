library(tidyverse)
library(usethis)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("depreciated/data/raw/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
scotland_delayed_transfer_of_care <-
  raw |>
  filter(`NHS Board` != "National") |>
  group_by(`NHS Board`) |>
  summarise(
    `Bed days occupied by delayed discharge patients` = max(`Bed days occupied by delayed discharge patients`, na.rm = TRUE)
  ) |>
  ungroup()

use_data(scotland_delayed_transfer_of_care, overwrite = TRUE)