##
## Wales hospital data
##
library(jsonlite)
library(httr)
library(RCurl)
library(dplyr)
library(readr)
library(tidyr)

##
## Helper function to download from Stats Wales
##
download.wales <- function(url) {
  # load initial data
  wimd_curr <- fromJSON(getURL(url), flatten=T)
  
  # put data into data.frame
  wimd_dat <- wimd_curr$value
  
  # get url of the first next page
  next_page <- wimd_curr$odata.nextLink
  
  # loop over the .json pages until we run out of data
  while(!is.null(next_page)) {
    wimd_curr <- fromJSON(getURL(next_page), flatten=T)  # download next batch of data
    
    wimd_dat <- bind_rows(wimd_dat, wimd_curr$value)  # append to data.frame
    next_page <- wimd_curr$odata.nextLink             # get url of next page (if there is one)
    
    print(next_page)  # track progress
  }
  
  wimd_dat
}

# ---- Performance against 4 hour waiting times target by hospital ----
# Source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Accident-and-Emergency
wait4_all <- download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0035")

# keep only latest stats
wait4 <- 
  wait4_all %>% 
  mutate(Date_SortOrder = as.integer(Date_SortOrder)) %>% 
  filter(Date_SortOrder == max(Date_SortOrder))

# save wide-format version of the data
wait4_wide <- 
  wait4 %>% 
  filter(Target_Code == "4hr") %>% 
  filter(!Hospital_ItemName_ENG %in% c("Major emergency departments", "Other emergency departments/minor injury units")) %>% 
  select(Date = Date_ItemName_ENG, Hospital = Hospital_ItemName_ENG, Measure_Code, Data) %>% 
  distinct() %>% 
  pivot_wider(names_from = Measure_Code, values_from = Data)
  
# save
wait4_wide %>% 
  write_csv("data/wales-ae.csv")


# ---- Delay reason by LHB Provider ----
# Source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Performance/Delayed-Transfers-of-Care
dtoc_all <- download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0810")

dtoc <- 
  dtoc_all %>% 
  mutate(Date_SortOrder = as.integer(Date_SortOrder)) %>% 
  filter(Date_SortOrder == max(Date_SortOrder))

dtoc_wide <- 
  dtoc %>% 
  select(Date = Date_ItemName_ENG, HB = Area_ItemName_ENG, Reason = Delayreason_ItemName_ENG, Data) %>% 
  mutate(Data = as.integer(Data)) %>% 
  group_by(Date, HB, Reason) %>% 
  summarise(Data = sum(Data)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Reason, values_from = Data)

dtoc_wide %>% 
  write_csv("data/wales-dtoc.csv")

# ---- NHS beds by organisation and site ----
# Source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Activity/NHS-Beds
beds_all <- download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0309")

beds <- 
  beds_all %>% 
  mutate(Year_SortOrder = as.integer(Year_SortOrder)) %>% 
  filter(Year_SortOrder == max(Year_SortOrder))

beds_wide <- 
  beds %>% 
  filter(Measure_ItemName_ENG == "Percentage occupancy" & Specialty_ItemName_ENG == "All Specialties") %>% 
  select(Date = Year_ItemName_ENG, HB = Organisation_ItemName_ENG, Year = Year_ItemName_ENG, Data) %>% 
  mutate(Data = as.numeric(Data)) %>% 
  # group_by(HB, Year) %>% 
  # summarise(Data = sum(Data)) %>% 
  pivot_wider(names_from = Year, values_from = Data)

beds_wide %>% 
  write_csv("data/wales-beds.csv")

# ---- Emergency ambulance calls and responses to red calls, by LHB and month ----
# Source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Performance/Ambulance-Services
ambo_all <- download.wales("http://open.statswales.gov.wales/en-gb/dataset/hlth1308")

ambo <- 
  ambo_all %>% 
  mutate(Date_SortOrder = as.integer(Date_SortOrder)) %>% 
  filter(Date_SortOrder == max(Date_SortOrder))

ambo_wide <- 
  ambo %>% 
  select(Date = Date_ItemName_ENG, HB = Area_ItemName_ENG, Measure_ItemName_ENG, Data) %>% 
  spread(Measure_ItemName_ENG, Data) %>% 
  filter(toupper(HB) != "WALES")  # get rid of 'total' row

ambo_wide %>% 
  write_csv("data/wales-ambo.csv")
