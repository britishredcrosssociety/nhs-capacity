# ---- Load libraries ----
library(tidyverse)
library(httr)
library(readxl)

# ---- LA Population ----
# Mid-2019: April 2019 local authority district population estimates
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
GET(
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2019localauthoritydistrictcodes/ukmidyearestimates20192019ladcodes.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

la_pop <- read_excel(tf, sheet = "MYE2 - Persons", skip = 4)

unlink(tf)
rm(tf)

# Calculate no. people aged 65+ in each LA
# sum whole population and people over 70
la_pop <-
  la_pop %>%
  filter(!Geography1 %in% c("Country", "Region") &
    str_sub(Code, 1, 1) == "E") %>%
  rename(`90` = `90+`) %>%
  pivot_longer(cols = `0`:`90`, names_to = "Age", values_to = "n_people") %>%
  mutate(Age = as.integer(Age)) %>%
  group_by(Code) %>%
  summarise(
    `No. people` = sum(n_people),
    `No. people aged 65+` = sum(n_people[Age >= 65], na.rm = TRUE)
  )

la_pop %>% 
  write_csv("data/raw/la_population.csv")

# ---- LA & County Names/Codes ----
# Local Authority Districts (December 2019) Names and Codes in the United Kingdom
# Source: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-names-and-codes-in-the-united-kingdom
la_names <-
  read_csv("https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv") %>%
  select(Code = LAD19CD, Name = LAD19NM)

# Counties (April 2019) Names and Codes in England
# Source: https://geoportal.statistics.gov.uk/datasets/counties-april-2019-names-and-codes-in-england
county_names <-
  read_csv("https://opendata.arcgis.com/datasets/b3d60eecd2e5483384fcd5cf03a82d27_0.csv") %>%
  select(Code = CTY19CD, Name = CTY19NM)

geog_names <-
  bind_rows(
    la_names,
    county_names
  )

geog_names %>% 
  write_csv("data/raw/la_county_names_codes.csv")
