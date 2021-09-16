# ---- Load libs ----
library(tidyverse)
library(sf)
library(geographr)
library(usethis)
library(rmapshaper)

# ---- Load performance data ----
england_performance <-
  read_rds("preprocess/data/england_performance.rds")

wales_performance <-
  read_rds("preprocess/data/wales_performance.rds")

scotland_performance <-
  read_rds("preprocess/data/scotland_performance.rds")

northern_ireland_performance <-
  read_rds("preprocess/data/northern_ireland_performance.rds")

# ---- Create Shape File ----
# Join Data
uk_shp <-
  bind_rows(
    england_performance |>
      select(geo_name = stp_name, geo_code = stp_code, geometry),
    wales_performance |>
      select(geo_name = lhb_name, geo_code = lhb_code, geometry),
    scotland_performance |>
      select(geo_name = hb_name, geo_code = hb_code, geometry),
    northern_ireland_performance |>
      select(geo_name = trust_name, geo_code = trust_code, geometry)
  )

# Minimise shapefile size to improve leaflet loading performance
uk_shp <-
  uk_shp |>
  ms_simplify()

# ---- Create UK data set ----
# Convert each performance data set to long format
england_long <-
  england_performance |>
  st_drop_geometry() |>
  rename(
    geo_name = stp_name,
    geo_code = stp_code
  ) |>
  pivot_longer(
    cols = !c(geo_name, geo_code),
    names_to = "variable",
    values_to = "score"
  ) |>
  mutate(nation = "England")

wales_long <-
  wales_performance |>
  st_drop_geometry() |>
  rename(
    geo_name = lhb_name,
    geo_code = lhb_code
  ) |>
  pivot_longer(
    cols = !c(geo_name, geo_code),
    names_to = "variable",
    values_to = "score"
  ) |>
  mutate(nation = "Wales") |>
  mutate(geo_name = str_remove_all(geo_name, " Health Board$"))

scotland_long <-
  scotland_performance |>
  st_drop_geometry() |>
  rename(
    geo_name = hb_name,
    geo_code = hb_code
  ) |>
  pivot_longer(
    cols = !c(geo_name, geo_code),
    names_to = "variable",
    values_to = "score"
  ) |>
  mutate(nation = "Scotland")

northern_ireland_long <-
  northern_ireland_performance |>
  st_drop_geometry() |>
  rename(
    geo_name = trust_name,
    geo_code = trust_code
  ) |>
  pivot_longer(
    cols = !c(geo_name, geo_code),
    names_to = "variable",
    values_to = "score"
  ) |>
  mutate(nation = "Nothern Ireland") |>
  mutate(geo_name = str_remove_all(geo_name, " Health and Social Care Trust$"))

# Join
uk_long_all_vars <-
  bind_rows(
    england_long,
    wales_long,
    scotland_long,
    northern_ireland_long
  )

# Drop empty rows
uk_long_dropped <-
  uk_long_all_vars |>
  drop_na()

# Move cols
uk_long <-
  uk_long_dropped |>
  relocate(nation, .after = geo_code)

# ---- Save to /data ----
use_data(uk_shp, overwrite = TRUE)
use_data(uk_long, overwrite = TRUE)