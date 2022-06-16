# ---- Load ----
library(tidyverse)
library(jsonlite)
library(httr)
library(RCurl)
library(tidyr)
library(lubridate)

# Function to scrape Wales data
download.wales <- function(url) {
  # load initial data
  wimd_curr <- fromJSON(getURL(url), flatten = T)

  # put data into data.frame
  wimd_dat <- wimd_curr$value

  # get url of the first next page
  next_page <- wimd_curr$odata.nextLink

  # loop over the .json pages until we run out of data
  while (!is.null(next_page)) {
    wimd_curr <- fromJSON(getURL(next_page), flatten = T) # download next batch of data

    wimd_dat <- bind_rows(wimd_dat, wimd_curr$value) # append to data.frame
    next_page <- wimd_curr$odata.nextLink # get url of next page (if there is one)

    print(next_page) # track progress
  }

  wimd_dat
}

# ---- Wrangle ----
raw <-
  download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth1308")

ambo <-
  raw |>
  as_tibble() |>
  mutate(Date_SortOrder = as.integer(Date_SortOrder)) |>
  filter(Date_SortOrder == max(Date_SortOrder))

wales_amublance_services <-
  ambo |>
  select(
    Date = Date_ItemName_ENG,
    HB_code = Area_Code,
    HB = Area_ItemName_ENG,
    Measure_ItemName_ENG,
    Data
  ) |>
  pivot_wider(names_from = Measure_ItemName_ENG, values_from = Data) |>
  filter(HB != "WALES") |>
  select(-Date)

wales_amublance_services |>
  write_rds("preprocess/data/wales_ambulance_services.rds")
