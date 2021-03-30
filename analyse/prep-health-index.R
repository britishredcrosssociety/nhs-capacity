# ---- Load libraries ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)
library(rmapshaper)

# ---- Load data ----
hi <- read_excel("data/health_index.xlsx", sheet = "2018", skip = 11)

# ---- Clean ----
hi_clean <- hi %>%
  slice(1:162) %>% 
  slice(-1:-2) %>% 
  select(
    county_ua_code = ...1,
    overall_health_index = ...6,
    healthy_lives = `Healthy Lives...9`,
    healthy_places = `Healthy Places...10`,
    healthy_people = `Healthy People...11`
  )  %>% 
  drop_na()

# Some areas have been combined. Separate them.
hi_missing <-
  hi_clean %>% 
filter(
    county_ua_code == "E06000052" | # Cornwall
    county_ua_code == "E09000012" | # Hackney
    county_ua_code == "E06000060"   # Buckinhamshire
    ) %>% 
mutate(
  county_ua_code = case_when(
    county_ua_code == "E06000052" ~ "E06000053", # Isles of Scilly
    county_ua_code == "E09000012" ~ "E09000001", # City of London
    county_ua_code == "E06000060" ~ "E10000002" # Buckinghamshire
  )
)

hi_complete <-
  bind_rows(
    hi_clean,
    hi_missing
  )

# ---- Join to shapefile and save ----
boundaries_counties_ua %>% 
  ms_simplify() %>% 
  filter(str_detect(county_ua_code, "^E")) %>% 
  left_join(
    hi_complete,
    by = "county_ua_code"
  ) %>% 
  relocate(geometry, .after = healthy_people) %>% 
  write_rds("app/data/health_index.rds")
