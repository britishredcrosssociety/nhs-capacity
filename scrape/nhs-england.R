# ---- Load libraries ----
library(tidyverse)
library(httr)
library(readxl)

# ---- A&E Attendance ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/Monthly-AE-December-2020.csv",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

a_and_e <- read_csv(tf)

unlink(tf)
rm(tf)

# ---- Ambulance Quality Indicators ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/AmbSYS-December-2020.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

ambulance <- read_excel(tf, sheet = "Response Times")

unlink(tf)
rm(tf)

# ---- Bed Occupancy ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

beds <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

unlink(tf)
rm(tf)