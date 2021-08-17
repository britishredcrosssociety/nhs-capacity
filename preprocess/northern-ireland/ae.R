library(tidyverse)
library(lubridate)

raw <-
  read_csv(
    "preprocess/data/raw/northern-ireland-ae-raw.csv"
  )

northern_ireland_ae <-
  raw |>
  filter(
    Date == max(Date) &
      Type == "Type 1"
  ) |>
  mutate(`Percent Under 4 Hours` = str_remove(`Percent Under 4 Hours`, "%") |> as.numeric()) |>
  group_by(Trust) |>
  filter(`Percent Under 4 Hours` == min(`Percent Under 4 Hours`)) |>
  ungroup() |>
  select(Month, Trust, `Percent Under 4 Hours`)

northern_ireland_ae |>
write_rds("preprocess/data/northern_ireland_ae.rds")