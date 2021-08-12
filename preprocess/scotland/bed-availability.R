library(tidyverse)
library(usethis)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("depreciated/data/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
scotland_bed_availability <-
  raw |>
  filter(`NHS Board` != "National") |>
  group_by(`NHS Board`) |>
  summarise(
    `Average number of available staffed beds` = min(`Average number of available staffed beds`, na.rm = TRUE),
  ) |>
  ungroup()

use_data(scotland_bed_availability, overwrite = TRUE)