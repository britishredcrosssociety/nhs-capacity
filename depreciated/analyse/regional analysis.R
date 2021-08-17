##
## Analyses in this script are to help with IL strategic planning
##
library(tidyverse)
library(geographr)
library(sf)

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

hi <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/england/health-index-2018.csv")

ics <- 
  read_csv(file = "depreciated/data/nhs-performance-england.csv")

lad_region <- read_sf("https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.geojson") |> 
  select(lad_code = LAD19CD, region_name = RGN19NM) |> 
  st_drop_geometry()

hi_london <- 
  hi |> 
  left_join(lad_region, by = c("county_ua_code" = "lad_code")) |> 
  filter(region_name == "London")

hi_london |> 
  select(county_ua_name, ends_with("_rank")) |> 
  arrange(desc(healthy_places_rank))

hi_london |> 
  select(county_ua_name, ends_with("_rank")) |> 
  arrange(desc(healthy_people_rank))

hi_london |> 
  select(county_ua_name, ends_with("_rank")) |> 
  arrange(desc(healthy_lives_rank))

ics <- 
  ics |> 
  # Calculate overall Trust performance, based on the quintiles
  rowwise() |>
  mutate(bin_sum = sum(c_across(`A&E performance (5 = worst performing 20%)`:`RTT performance (5 = worst performing 20%)`), na.rm = TRUE)) |> 
  ungroup() |> 
  
  mutate(
    sum_of_5s = count_if_worst(`A&E performance (5 = worst performing 20%)`) +
      count_if_worst(`Bed occupancy performance (5 = worst performing 20%)`) +
      count_if_worst(`Cancer waiting list performance (5 = worst performing 20%)`) +
      count_if_worst(`RTT performance (5 = worst performing 20%)`)
  ) |> 
  
  arrange(desc(sum_of_5s))

ics_london <- 
  ics |> 
  filter(`NHS region` == "London")
  
