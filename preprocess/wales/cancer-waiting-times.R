# ---- Load ----
library(tidyverse)
library(jsonlite)
library(httr)
library(RCurl)
library(tidyr)
library(lubridate)
library(usethis)

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
  download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0055")

wales_cancer_times <-
  raw |>
  as_tibble() |>
  mutate(Month_SortOrder = as.integer(Month_SortOrder)) |>
  filter(
    Month_SortOrder == max(Month_SortOrder) &
      Localhealthboard_ItemName_ENG != "Wales" &
      Agegroup_ItemName_ENG == "Total" &
      Sex_ItemName_ENG == "Total" &
      Tumoursite_ItemName_ENG == "All sites" &
      str_detect(Measure_ItemName_ENG, "percentage")
  ) |>
  select(
    HB_code = Localhealthboard_Code,
    HB = Localhealthboard_ItemName_ENG,
    `% starting treatment within 62 days` = Data
  )

use_data(wales_cancer_times, overwrite = TRUE)