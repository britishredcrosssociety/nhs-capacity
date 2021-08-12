library(tidyverse)
library(httr)
library(readxl)
library(usethis)

GET(
  "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-emergency-care-tables-19-20.xls",
  write_disk(tf <- tempfile(fileext = "xls"))
)

raw <-
  read_excel(
    tf,
    sheet = "Table 22",
    range = "A4:F29"
  )

# Clean up columns and keep only Trusts
northern_ireland_reattend <-
  raw |>
  rename(Trust = ...1, Reattend = `2019/20`) |>
  select(Trust, Reattend) |>
  filter(Trust %in% c("Belfast Trust", "Northern", "South Eastern", "Southern Trust", "Western Trust")) |>
  mutate(Trust = gsub(" [0-9]", "", Trust)) |> # get rid of footnotes
  mutate(Reattend = as.numeric(Reattend)) |>
  mutate(Reattend = round(Reattend * 100, 1)) |>
  mutate(Trust = str_remove(Trust, " Trust"))

use_data(northern_ireland_reattend, overwrite = TRUE)