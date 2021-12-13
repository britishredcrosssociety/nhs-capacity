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
  download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0092")

# Find max data code and update below
# raw |> as_tibble() |> pull(Date_Code) |> max()
bed_availability <-
  raw |>
  as_tibble() |>
  filter(
    str_detect(Date_Code, "^20211210") &
      LocalHealthBoard_ItemName_ENG != "Wales" &
      HospitalType_Code == "NHS" &
      Indicator_ItemName_ENG %in% c("General and acute beds available&#10;", "General and acute beds occupied&#10;")
  )

wales_bed_availability <-
  bed_availability |>
  select(
    Day = Date_Code,
    HB_code = LocalHealthBoard_Code,
    HB = LocalHealthBoard_ItemName_ENG,
    Indicator_ItemName_ENG, Data
  ) |>
  mutate(
    Data = as.numeric(Data),
    Day = ymd(Day)
  ) |>
  pivot_wider(names_from = Indicator_ItemName_ENG, values_from = Data) |>
  group_by(Day, HB_code, HB) |>
  mutate(`% general and acute beds occupied` = `General and acute beds occupied&#10;` / `General and acute beds available&#10;`) |>
  ungroup() |>
  mutate(Date = paste0(month.name[month(Day)], " ", year(Day))) |>
  group_by(Date, HB_code, HB) |>
  summarise(`% general and acute beds occupied` = mean(`% general and acute beds occupied`, na.rm = TRUE)) |>
  ungroup() |>
  select(-Date)

wales_bed_availability |>
  write_rds("preprocess/data/wales_bed_availability.rds")