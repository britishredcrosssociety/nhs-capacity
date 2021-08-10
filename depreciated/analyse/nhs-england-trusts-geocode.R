# ---- Load libraries ----
library(tidyverse)
library(sf)
library(geographr)

# ---- Load and Save data ----
# The geographr package is large, and should not be called into a Shiny app.
# Save the NHS Trust points.

# Save to data folder
points_nhs_trusts |>
  rename(
    org_code = nhs_trust_code,
    org_name = nhs_trust_name
  ) |>
  # Remove Welsh Trusts
  filter(org_code != "RQF" & org_code != "RT4" & org_code != "RYT") |>
  write_sf("data/points_nhs_trusts.geojson")

# Save only open trusts to app
open_trusts <-
  points_nhs_trusts |>
  rename(
    org_code = nhs_trust_code,
    org_name = nhs_trust_name
  ) |>
  # Remove Welsh Trusts
  filter(org_code != "RQF" & org_code != "RT4" & org_code != "RYT") |>
  filter(status == "open") |>
  mutate(
    org_name = str_to_title(org_name),
    org_name = str_replace(org_name, "Nhs", "NHS")
  )

open_trusts |>
  write_sf("app/data/open_nhs_trusts_points.geojson")