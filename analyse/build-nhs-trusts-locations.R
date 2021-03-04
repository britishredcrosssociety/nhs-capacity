# ---- Load libraries ----
library(tidyverse)
library(readxl)
library(httr)
library(tidygeocoder)

# ---- Load data ----
# Source:
# - https://digital.nhs.uk/services/organisation-data-service/data-downloads/other-nhs-organisations
# - see page 85 of the Standards Repository Document for metadata

# Other potential (unused) sources of Trusts:
# - https://www.england.nhs.uk/publication/nhs-provider-directory/
# - https://odsportal.digital.nhs.uk/Organisation/Search
# - https://www.nhs.uk/ServiceDirectories/Pages/NHSTrustListing.aspx

# Load raw trust data
GET(
  "https://files.digital.nhs.uk/assets/ods/current/etr.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)
unzip(tf, exdir = tempdir())
unlink(tf)

trusts_raw <- read_csv(
  file = list.files(
    tempdir(),
    pattern = "*.csv",
    full.names = TRUE
  ),
  col_names = c(
    "org_code",
    "name",
    "national_grouping",
    "high_level_health_geography",
    "address_1",
    "address_2",
    "address_3",
    "address_4",
    "address_5",
    "postcode",
    "open_date",
    "close_date"
  )
)

# ---- Wrangle ----
# Create open status
trusts_clean <-
  trusts_raw %>%
  mutate(status = if_else(is.na(close_date), "open", "closed")) %>%
  select(
    -open_date,
    -close_date
  )

# ---- Geocode ----
trusts_geocoded <-
  trusts_clean %>%
  geocode(
    postalcode = postcode,
    method = "osm"
  )

trusts_geocoded %>%
  write_csv("data/nhs-trusts-geocoded.csv")
