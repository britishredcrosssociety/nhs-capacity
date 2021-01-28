# ---- Load libraries ----
library(tidyverse)
library(httr)
library(readxl)
library(janitor)

# ---- A&E Attendance ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/December-2020-AE-by-provider-8c90a.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

eng_ae <- read_excel(tf, sheet = "Provider Level Data", skip = 15)

unlink(tf)
rm(tf)

# remove first two entries (one is totals, other is blank)
eng_ae <- eng_ae %>%
  slice(-(1:2))

eng_ae <- eng_ae %>%
  select(
    Code,
    `Total Attendances > 4 hours`,
    `Percentage in 4 hours or less (type 1)`,
    `Percentage in 4 hours or less (all)`,
    `Number of patients spending >4 hours from decision to admit to admission`,
    `Number of patients spending >12 hours from decision to admit to admission`
  )

# Save to raw
eng_ae %>% 
  write_csv("data/raw/nhs_eng_ae.csv")

# ---- Ambulance Quality Indicators ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/AmbSYS-December-2020.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

# column names and types to use for loading all data
ambo_colnames <- c("Code", "Ambulance Service", "Count of Incidents", "Blank", "Total (hours)", "Mean (min:sec)", "90th centile (min:sec)")
ambo_types <- c("text", "text", "numeric", "numeric", "numeric", "date", "date")


# Category 1
eng_ambo_cat1 <- read_excel(tf,
  sheet = "Response Times", range = "C8:I18",
  col_names = ambo_colnames, col_types = ambo_types
) %>%
  remove_empty("cols") %>%
  mutate(category = "cat1")

# Category 1T
eng_ambo_cat1t <- read_excel(tf,
  sheet = "Response Times", range = "C22:I32",
  col_names = ambo_colnames, col_types = ambo_types
) %>%
  remove_empty("cols") %>%
  mutate(category = "cat1t")

# Category 2
eng_ambo_cat2 <- read_excel(tf,
  sheet = "Response Times", range = "C36:I46",
  col_names = ambo_colnames, col_types = ambo_types
) %>%
  remove_empty("cols") %>%
  mutate(category = "cat2")

# Category 3
eng_ambo_cat3 <- read_excel(tf,
  sheet = "Response Times", range = "C50:I60",
  col_names = ambo_colnames, col_types = ambo_types
) %>%
  remove_empty("cols") %>%
  mutate(category = "cat3")

# Category 4
eng_ambo_cat4 <- read_excel(tf,
  sheet = "Response Times", range = "C64:I74",
  col_names = ambo_colnames, col_types = ambo_types
) %>%
  remove_empty("cols") %>%
  mutate(category = "cat4")

unlink(tf)
rm(tf)

# combine stats
eng_ambo <- bind_rows(
  eng_ambo_cat1,
  eng_ambo_cat1t,
  eng_ambo_cat2,
  eng_ambo_cat3,
  eng_ambo_cat4
)

# Reformat dates
eng_ambo <-
  eng_ambo %>% 
  mutate(`Mean (min:sec)` = format(`Mean (min:sec)`, format= "%M:%S"),
         `90th centile (min:sec)` = format(`90th centile (min:sec)`, format= "%M:%S"))


# Save to raw
eng_ambo %>% 
  write_csv("data/raw/nhs_eng_ambulance.csv")

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

# save to raw
eng_beds %>% 
  write_csv("data/raw/nhs_eng_beds.csv")

# ---- DToC ----
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Trust-Type-B-February-2020-4W5PA.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

eng_dtoc <- read_excel(tf, sheet = "Trust - by responsible org", skip = 13)

unlink(tf)
rm(tf)

eng_dtoc <- eng_dtoc %>% 
  slice(-(1:2)) %>% 
  remove_empty("cols") %>% 
  select(Code, `NHS DToC days` = NHS...5, `Social Care DToC days` = `Social Care...6`)

# Save to raw
eng_dtoc %>% 
  write_csv("data/raw/nhs_eng_dtoc.csv")
