# ---- Purpose ----
# This script should be run to generate the raw data used by other scripts in
# /preprocess/scotland/

# ---- Load ----
library(tidyverse)
library(lubridate)
library(readxl)

# List of hospitals from https://www.isdscotland.org/Health-Topics/Hospital-Care/Hospitals/
# Hospital IDs came from manual matching with hospital URLs on http://www.nhsperforms.scot/
scotland_hospitals <-
  read_excel(
    "preprocess/data/raw/scotland-hospital-indicator-list.xlsx",
    sheet = "Hospitals"
  )

# List of indicators from http://www.nhsperforms.scot/
# Indicator IDs came from manual matching with indicator URLs on http://www.nhsperforms.scot/
# (you'll need to click on any hospital first to see the full set of available data)
scotland_indicators <-
  read_excel(
    "preprocess/data/raw/scotland-hospital-indicator-list.xlsx",
    sheet = "Indicators"
  )

scotland_hospitals <-
  scotland_hospitals |>
  filter(!is.na(HospitalID))

# ---- Download indicators ----
# The url takes the form:
# http://www.nhsperforms.scot/hospital-data/indicator-hospital/csv?hospitalid=42&indicatorid=1
base_url <- "http://www.nhsperforms.scot/hospital-data/indicator-hospital/csv?"

# Download a .csv file for each indicator in each hospital
# ** this takes a long time **
for (hosp_id in scotland_hospitals$HospitalID) {
  for (ind_id in scotland_indicators$IndicatorID) {
    url <- paste0(
      base_url,
      "hospitalid=",
      hosp_id,
      "&indicatorid=",
      ind_id
    )

    out_file <-
      paste0(
        "hospital ",
        hosp_id,
        " - ind ",
        ind_id,
        ".csv"
      )

    if (!file.exists(file.path("preprocess/data/raw/scotland-raw-data", out_file))) {
      download.file(url,
        file.path("preprocess/data/raw/scotland-raw-data", out_file),
        mode = "wb"
      )
    }
  }
}

# ---- Process indicators ----
# Open each of the downloaded files and grab the `HospitalValue` for the most recent date (first row)
# then merge all into a single file
for (hosp_id in scotland_hospitals$HospitalID) {
  for (ind_id in scotland_indicators$IndicatorID) {
    tmp_file <-
      paste0(
        "hospital ",
        hosp_id,
        " - ind ",
        ind_id,
        ".csv"
      )

    tmp_stats <-
      read_csv(
        file.path(
          "preprocess/data/raw/scotland-raw-data",
          tmp_file
        )
      )

    tmp_stats <- tmp_stats[1, ] # keep first row (most recent stat)

    if (!"HospitalValue" %in% names(tmp_stats)) {
      tmp_stats$HospitalValue <- NA
    }

    tmp_stats <-
      tmp_stats |>
      mutate(
        HospitalValue = as.double(HospitalValue),
        BoardValue = as.double(BoardValue)
      ) |>
      select(Date, HospitalValue, BoardValue)

    if (!exists("scot_stats")) {
      scot_stats <-
        bind_cols(
          data_frame(
            HospitalID = hosp_id,
            IndicatorID = ind_id
          ),
          tmp_stats
        )
    } else {
      scot_stats <-
        bind_rows(
          scot_stats,
          bind_cols(
            tibble(HospitalID = hosp_id, IndicatorID = ind_id),
            tmp_stats
          )
        )
    }
  }

  print(paste0("Finished hospital ", hosp_id))
}

scot_stats |>
  write_csv("preprocess/data/raw/scotland-raw-data/all-scotland-stats.csv")

# If hospital value is NA, use the board value
scot_stats$HospitalValue <-
  ifelse(
    is.na(scot_stats$HospitalValue),
    scot_stats$BoardValue,
    scot_stats$HospitalValue
  )

# Merge indicator names
scot_stats <-
  scot_stats |>
  left_join(
    scotland_indicators |> select(IndicatorID, IndicatorName),
    by = "IndicatorID"
  )

# Widen the data so only one row per hospital and keep the most recent indicator
# in each hospital
scot_stats_wide <-
  scot_stats |>
  select(Date, HospitalID, IndicatorName, HospitalValue) |>
  mutate(Date = dmy(Date)) |>
  group_by(HospitalID, IndicatorName) |>
  filter(Date == max(Date)) |>
  ungroup() |>
  pivot_wider(
    id_cols = HospitalID,
    names_from = IndicatorName,
    values_from = HospitalValue
  )

# merge indicators into main hospital table
scotland_hospitals_stats <-
  scotland_hospitals |>
  select(HospitalID, `NHS Board`, `Location Code`, `Location Name`) |>
  left_join(scot_stats_wide, by = "HospitalID")

scotland_hospitals_stats |>
  write_csv("preprocess/data/raw/scotland-hospital-indicators.csv")
