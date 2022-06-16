library(tidyverse)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("preprocess/data/raw/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
scotland_delayed_transfer_of_care <-
  raw |>
  filter(`NHS Board` != "Golden Jubilee Foundation") |>
  group_by(`NHS Board`) |>
  summarise(
    `Bed days occupied by delayed discharge patients` = max(`Bed days occupied by delayed discharge patients`, na.rm = TRUE)
  ) |>
  ungroup()

scotland_delayed_transfer_of_care |>
  write_rds("preprocess/data/scotland_delayed_transfer_of_care.rds")
