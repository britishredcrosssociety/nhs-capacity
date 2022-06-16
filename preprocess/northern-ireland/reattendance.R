library(tidyverse)
library(httr)
library(readxl)

GET(
  "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-emergency-care-tables-20-21.xls",
  write_disk(tf <- tempfile(fileext = "xls"))
)

raw <-
  read_excel(
    tf,
    sheet = "Table 22",
    range = "A4:F27"
  )

# Clean up columns and keep only Trusts
northern_ireland_reattendance <-
  raw |>
  select(Trust = ...1, Reattend = `2020/21`) |>
  filter(str_detect(Trust, "Trust$")) |>
  # filter(Trust %in% c("Belfast Trust", "Northern Trust", "South Eastern Trust", "Southern Trust", "Western Trust")) |>
  mutate(Trust = gsub(" [0-9]", "", Trust)) |> # get rid of footnotes
  mutate(Reattend = as.numeric(Reattend)) |>
  mutate(Reattend = round(Reattend * 100, 1)) |>
  mutate(Trust = str_remove(Trust, " Trust"))

northern_ireland_reattendance |>
  write_rds("preprocess/data/northern_ireland_reattendance.rds")
