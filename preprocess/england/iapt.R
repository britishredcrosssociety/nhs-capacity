# Load libs
library(tidyverse)
library(httr)

# Load data
# Download latest quarterly data by STP/ICS from https://digital.nhs.uk/data-and-information/publications/statistical/psychological-therapies-report-on-the-use-of-iapt-services
GET(
  "https://files.digital.nhs.uk/F4/95AF18/iapt_quarterly_STP_and_CCGs_q1_2122.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

iapt_raw <-
  read_csv(
    file.path(tempdir(), "iapt_quarterly_STP_and_CCGs_q1_2122.csv"),
    col_types = cols(.default = col_character())
  )

# Analyse waiting times by STP
iapt_filtered <-
  iapt_raw |>
  filter(
    CCG == "All_CCG",
    VariableType == "Total",
    GroupType == "STP",
    STP != "InvalidCode",
    MEASURE_NAME == "Percentage_FirstTreatment18WeeksFinishedCourseTreatment"
  )

iapt_selected <-
  iapt_filtered |>
  select(
    stp_code_short = STP,
    `Referrals that finished a course of treatment in the month waiting 126 days or less for first treatment` = MEASURE_VALUE_SUPPRESSED
  )

iapt_double <-
  iapt_selected |>
  mutate(`Referrals that finished a course of treatment in the month waiting 126 days or less for first treatment` = as.double(`Referrals that finished a course of treatment in the month waiting 126 days or less for first treatment`))

england_iapt <-
  iapt_selected

# Save
england_iapt |>
  write_rds("preprocess/data/england_iapt.rds")