library(tidyverse)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("preprocess/data/raw/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
scotland_bed_availability <-
  raw |>
  filter(`NHS Board` != "Golden Jubilee Foundation") |>
  group_by(`NHS Board`) |>
  summarise(
    `Average number of available staffed beds` = min(`Average number of available staffed beds`, na.rm = TRUE),
  ) |>
  ungroup()

scotland_bed_availability |>
  write_rds("preprocess/data/scotland_bed_availability.rds")
