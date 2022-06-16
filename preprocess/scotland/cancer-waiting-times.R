library(tidyverse)

# Raw data generated from preprocess/scotland/scrape-indicators.R
raw <-
  read_csv("preprocess/data/raw/scotland-hospital-indicators.csv")

# Pick worst performance stats in each Health Board
scotland_cancer_waiting_times <-
  raw |>
  filter(`NHS Board` != "Golden Jubilee Foundation") |>
  group_by(`NHS Board`) |>
  summarise(
    `Average number of days waited from receipt of an urgent referral with suspicion of cancer to first cancer treatment (62 day standard)` = max(`Average number of days waited from receipt of an urgent referral with suspicion of cancer to first cancer treatment (62 day standard)`, na.rm = TRUE),
  ) |>
  ungroup()

scotland_cancer_waiting_times |>
  write_rds("preprocess/data/scotland_cancer_waiting_times.rds")
