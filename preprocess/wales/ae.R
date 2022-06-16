# ---- Load ----
library(jsonlite)
library(httr)
library(RCurl)
library(dplyr)
library(readr)
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
raw <- download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0035")

# keep only latest stats
ae <-
  raw |>
  mutate(Date_SortOrder = as.integer(Date_SortOrder)) |>
  filter(Date_SortOrder == max(Date_SortOrder))

# save wide-format version of the data
wales_ae <-
  ae |>
  filter(Target_Code == "4hr") |>
  filter(!Hospital_ItemName_ENG %in% c("Major emergency departments", "Other emergency departments/minor injury units")) |>
  select(
    Hospital_code = Hospital_Code,
    Hospital = Hospital_ItemName_ENG,
    Measure_Code, Data
  ) |>
  distinct() |>
  pivot_wider(names_from = Measure_Code, values_from = Data)

wales_ae |>
  write_rds("preprocess/data/wales_ae.rds")
