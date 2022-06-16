library(tidyverse)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("preprocess/data/raw/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
scotland_ae <-
  raw |>
  filter(`NHS Board` != "Golden Jubilee Foundation") |>
  group_by(`NHS Board`) |>
  summarise(
    `Percentage seen within 4 hours` = min(`Percentage seen within 4 hours`, na.rm = TRUE)
  ) |>
  ungroup()

scotland_ae |>
  write_rds("preprocess/data/scotland_ae.rds")
