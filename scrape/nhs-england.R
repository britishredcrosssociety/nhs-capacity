# ---- Load libraries ----
library(tidyverse)
library(httr)
library(readxl)
library(janitor)

# ---- A&E Attendance ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/Monthly-AE-December-2020.csv",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

eng_a_and_e <- read_csv(tf)

unlink(tf)
rm(tf)

# ---- Ambulance Quality Indicators ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/AmbSYS-December-2020.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

# column names and types to use for loading all data
ambo_colnames  <- c("Code", "Ambulance Service", "Count of Incidents", "Blank", "Total (hours)","Mean (min:sec)", "90th centile (min:sec))")
ambo_types <- c("text", "text", "numeric", "numeric", "numeric", "date", "date")


# Category 1 
eng_ambo_cat1 <- read_excel(tf,
                           sheet = "Response Times", range = "C8:I18", 
                           col_names = ambo_colnames, col_types = ambo_types) %>% 
  remove_empty("cols") %>% 
  mutate(category = "cat1")

# Category 1T
eng_ambo_cat1t <- read_excel(tf,
                            sheet = "Response Times", range = "C22:I32", 
                            col_names = ambo_colnames, col_types = ambo_types) %>% 
  remove_empty("cols") %>% 
  mutate(category = "cat1t")

# Category 2
eng_ambo_cat2 <- read_excel(tf,
                           sheet = "Response Times", range = "C36:I46", 
                           col_names = ambo_colnames, col_types = ambo_types) %>% 
  remove_empty("cols") %>% 
  mutate(category = "cat2")

# Category 3
eng_ambo_cat3 <- read_excel(tf,
                           sheet = "Response Times", range = "C50:I60", 
                           col_names = ambo_colnames, col_types = ambo_types) %>% 
  remove_empty("cols") %>% 
  mutate(category = "cat3")

# Category 4
eng_ambo_cat4 <- read_excel(tf,
                           sheet = "Response Times", range = "C64:I74", 
                           col_names = ambo_colnames, col_types = ambo_types) %>% 
  remove_empty("cols") %>% 
  mutate(category = "cat4")

# combine stats
eng_ambo <- bind_rows(
  eng_ambo_cat1,
  eng_ambo_cat1t,
  eng_ambo_cat2,
  eng_ambo_cat3,
  eng_ambo_cat4
)

unlink(tf)
rm(tf)

# ---- Bed Occupancy ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

eng_beds <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
eng_beds <- eng_beds %>% 
  slice(-(1:2))

eng_beds <- eng_beds %>% 
  select(Code = `Org Code`, `Beds Occupied (%)` = Total...18)
