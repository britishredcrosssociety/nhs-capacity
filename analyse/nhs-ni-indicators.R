library(tidyverse)
library(lubridate)
library(readxl)
library(httr)

# Hospital locations manually scraped from each page on each Trust, linked from: http://online.hscni.net/hospitals/health-and-social-care-trusts/
# ni_hosp <- read_excel(file.path(nhs.dir, "NI", "NI Hospitals.xlsx"))

# ---- Emergency care waiting times ----
# Need to download from the "Interactive Emergency Care Data" site: https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-ecwt-data-q4-20-21.html
# ... accessed via https://www.health-ni.gov.uk/articles/emergency-care-waiting-times
ae <- read_csv("data/ni-raw-data/Emergency Care Waiting Times.csv")

# Keep only the most recent, worst-performing stats for each Trust
ae <-  
  ae %>%
  filter(
    Date == max(Date) &
      Type == "Type 1"
  ) %>% 
  mutate(`Percent Under 4 Hours` = str_remove(`Percent Under 4 Hours`, "%") %>% as.numeric()) %>% 
  
  group_by(Trust) %>% 
  filter(`Percent Under 4 Hours` == min(`Percent Under 4 Hours`)) %>% 
  ungroup() %>% 
  
  select(Month, Trust, `Percent Under 4 Hours`)

ae %>% 
  write_csv("data/ni-ae.csv")

# ---- Emergency care activity 2019/20 ----
# Table 22: Percentage of Attendances Who Re-attended within 7 Days
# https://www.health-ni.gov.uk/articles/emergency-care-and-ambulance-statistics
GET("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-emergency-care-tables-19-20.xls",
    write_disk(tf <- tempfile(fileext = "xls")))

reattend <- read_excel(tf, sheet = "Table 22", range = "A4:F29")

# Clean up columns and keep only Trusts
reattend <- 
  reattend %>% 
  rename(Trust = ...1, Reattend = `2019/20`) %>% 
  select(Trust, Reattend) %>% 
  
  filter(Trust %in% c("Belfast Trust", "Northern", "South Eastern", "Southern Trust", "Western Trust")) %>% 
  
  mutate(Trust = gsub(" [0-9]", "", Trust)) %>%   # get rid of footnotes
  mutate(Reattend = as.numeric(Reattend)) %>% 
  mutate(Reattend = round(Reattend * 100, 1)) %>% 
  
  mutate(Trust = str_remove(Trust, " Trust"))

reattend %>% 
  write_csv("data/ni-reattendance.csv")

unlink(tf)

# ---- Cancer waiting lists ----
# Source: https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-cancer-waiting-times-january-march-2021
cancer <- read_csv("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-cwt-62-day-wait-by-trust-q4-20-21.csv")

cancer <- 
  cancer %>% 
  rename(Month = `Treatment Month`, Trust = `HSC Trust`) %>% 
  mutate(Month = dmy(glue::glue("01-{Month}"))) %>% 
  filter(Month == max(Month))

cancer %>% 
  write_csv("data/ni-cancer.csv")

# ---- Waiting lists ----
# Statistics by HSC Trust and Outpatients: https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-outpatient-waiting-times-march-2021
ni_outpatient <- read_csv("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-outpatients-q4-20-21.csv",
                          col_types = cols(
                            .default = col_character(),
                            `Quarter Ending` = col_character(),
                            `HSC Trust` = col_character(),
                            Specialty = col_character(),
                            `Programme of Care` = col_character()
                          ))

ni_outpatient <- 
  ni_outpatient %>% 
  select(-X25, -X26) %>% 
  rename(Trust = `HSC Trust`) %>% 
  
  # Remove commas from the data columns
  mutate(across(`0 - 6 weeks`:`Total Waiting`, ~as.numeric(str_remove(.x, ",")))) %>% 
  
  mutate(Date = dmy(`Quarter Ending`)) %>%
  relocate(Date) %>% 
  filter(Date == max(Date, na.rm = TRUE)) %>% 
  
  group_by(Date, Trust, Specialty) %>% 
  summarise(
    `Total waiting > 18 weeks` = sum(`>18-52 weeks`, na.rm = TRUE) + sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting > 52 weeks` = sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting` = sum(`Total Waiting`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  
  group_by(Date, Trust) %>% 
  summarise(
    `Outpatient: Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE),
    `Outpatient: Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
    `Outpatient: Total waiting` = sum(`Total waiting`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  
  mutate(
    `Outpatient: % waiting > 18 weeks` = `Outpatient: Total waiting > 18 weeks` / `Outpatient: Total waiting`,
    `Outpatient: % waiting > 52 weeks` = `Outpatient: Total waiting > 52 weeks` / `Outpatient: Total waiting`
  )

ni_outpatient %>% 
  write_csv("data/ni-rtt-outpatients.csv")

# Inpatient & Day Case waiting times: https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-inpatient-and-day-case-waiting-times-march-2021
ni_inpatient <- read_csv("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-total-waiting-q4-20-21.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Quarter Ending` = col_character(),
                           `HSC Trust` = col_character(),
                           Specialty = col_character(),
                           `Programme of Care` = col_character()
                         ))

ni_inpatient <- 
  ni_inpatient %>% 
  rename(Trust = `HSC Trust`) %>% 
  mutate(Date = dmy(`Quarter Ending`)) %>% 
  filter(Date == max(Date, na.rm = TRUE)) %>% 
    
    group_by(Date, Trust, Specialty) %>% 
    summarise(
      `Total waiting > 52 weeks` = sum(`Sum of >52 weeks`, na.rm = TRUE),
      `Total waiting` = sum(`Sum of Total`, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    
    group_by(Date, Trust) %>% 
    summarise(
      `Inpatient and day case: Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
      `Inpatient and day case: Total waiting` = sum(`Total waiting`, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    
    mutate(`Inpatient and day case: % waiting > 52 weeks` = `Inpatient and day case: Total waiting > 52 weeks` / `Inpatient and day case: Total waiting`)

ni_inpatient %>% 
  write_csv("data/ni-rtt-inpatients.csv")
