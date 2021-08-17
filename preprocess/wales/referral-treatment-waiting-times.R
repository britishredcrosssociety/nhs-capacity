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
  download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0083")

rtt <-
  raw |>
  as_tibble() |>
  mutate(Date_SortOrder = as.integer(Date_SortOrder)) |>
  filter(
    Date_SortOrder == max(Date_SortOrder)
  ) |>
  select(
    HB_code = LHBProvider_Code,
    HB = LHBProvider_ItemName_ENG,
    WeeksWaiting_SortOrder, Data
  ) |>
  mutate(WeeksWaiting_SortOrder = as.integer(WeeksWaiting_SortOrder))

# Calculate total across all TreatmentFunction_ItemName_ENG items
rtt_total <-
  rtt |>
  group_by(HB_code, HB, WeeksWaiting_SortOrder) |>
  summarise(Data = sum(Data)) |>
  ungroup() |>
  arrange(HB, WeeksWaiting_SortOrder)

# Calculate 18+ weeks (WeeksWaiting_SortOrder >= 20) and 52+ weeks (WeeksWaiting_SortOrder >= 48), as well as total waiting list sizes
wales_referral_treatment_waiting_times <-
  rtt_total |>
  pivot_wider(names_from = WeeksWaiting_SortOrder, values_from = Data) |>
  rowwise() |>
  mutate(
    `Total on waiting list` = sum(c_across(`2`:`61`), na.rm = TRUE),
    `Waiting 18+ weeks` = sum(c_across(`20`:`61`), na.rm = TRUE),
    `Waiting 53+ weeks` = sum(c_across(`48`:`61`), na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    `% waiting 18+ weeks` = `Waiting 18+ weeks` / `Total on waiting list`,
    `% waiting 53+ weeks` = `Waiting 53+ weeks` / `Total on waiting list`
  ) |>
  select(
    HB_code, HB,
    `Total on waiting list`,
    `Waiting 18+ weeks`,
    `Waiting 53+ weeks`,
    `% waiting 18+ weeks`,
    `% waiting 53+ weeks`
  )

wales_referral_treatment_waiting_times |>
write_rds("preprocess/data/wales_referral_treatment_waiting_times.rds")