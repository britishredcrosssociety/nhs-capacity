# ---- Load libraries ----
library(tidyverse)
library(sf)
library(geographr)

# ---- Load and Save data ----
# The geographr package is large, and should not be called into a Shiny app.
# Save the NHS Trust points.
points_nhs_trusts %>%
  rename(
    org_code = nhs_trust_code,
    org_name = nhs_trust_name
  ) %>% 
write_sf("data/points_nhs_trusts.geojson")
