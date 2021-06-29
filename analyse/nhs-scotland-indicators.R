##
## download set of .csv files for Scotland
##
library(tidyverse)
library(readxl)

# list of hospitals from https://www.isdscotland.org/Health-Topics/Hospital-Care/Hospitals/
# - hospital IDs came from manual matching with hospital URLs on http://www.nhsperforms.scot/
scot_hosp <- read_excel("data/NHS Scotland indicators.xlsx", sheet = "Hospitals")

# list of indicators from http://www.nhsperforms.scot/
# - indicator IDs came from manual matching with indicator URLs on http://www.nhsperforms.scot/
#   (you'll need to click on any hospital first to see the full set of available data)
scot_inds <- read_excel("data/NHS Scotland indicators.xlsx", sheet = "Indicators")

scot_hosp <- 
  scot_hosp %>% 
  filter(!is.na(HospitalID))

# the url takes the form:
# http://www.nhsperforms.scot/hospital-data/indicator-hospital/csv?hospitalid=42&indicatorid=1
base_url <- "http://www.nhsperforms.scot/hospital-data/indicator-hospital/csv?"

for (hosp_id in scot_hosp$HospitalID) {
  for (ind_id in scot_inds$IndicatorID) {
    url <- paste0(base_url, "hospitalid=", hosp_id, "&indicatorid=", ind_id)
    
    out_file <- paste0("hospital ", hosp_id, " - ind ", ind_id, ".csv")
    
    if (!file.exists(file.path("data/scotland-raw-data", out_file))) {
      download.file(url,
                    file.path("data/scotland-raw-data", out_file),
                    mode = "wb")
    }
  }
}

# open each of the downloaded files and grab the `HospitalValue` for the most recent date (first row)
# then merge all into a single file
for (hosp_id in scot_hosp$HospitalID) {
  for (ind_id in scot_inds$IndicatorID) {
    tmp_file <- paste0("hospital ", hosp_id, " - ind ", ind_id, ".csv")
    tmp_stats <- read_csv(file.path("data/scotland-raw-data", tmp_file))
    
    tmp_stats <- tmp_stats[1,]  # keep first row (most recent stat)
    
    if (!"HospitalValue" %in% names(tmp_stats)) {
      tmp_stats$HospitalValue <- NA
    }
    
    tmp_stats <- 
      tmp_stats %>% 
      mutate(
        HospitalValue = as.double(HospitalValue),
        BoardValue    = as.double(BoardValue)
      ) %>% 
      select(Date, HospitalValue, BoardValue)
    
    if (!exists("scot_stats")) {
      scot_stats = bind_cols(data_frame(HospitalID = hosp_id, IndicatorID = ind_id), tmp_stats)
    } else {
      scot_stats = bind_rows(
        scot_stats,
        
        bind_cols(
          data_frame(HospitalID = hosp_id, IndicatorID = ind_id), 
          tmp_stats
        )
      )
    }
    
  }
  
  print(paste0("Finished hospital ", hosp_id))
}

write_csv(scot_stats, file.path(nhs.dir, "Scotland", "Scotland hospital stats.csv"))

##
## extract postcodes for each hospital
##
# regular expression to match postcodes (allowing lowercase and unlimited spaces)
# source: https://stackoverflow.com/a/7259020
# see also: page 6 of https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/488478/Bulk_Data_Transfer_-_additional_validation_valid_from_12_November_2015.pdf
postcode_regex = "(([gG][iI][rR] {0,}0[aA]{2})|((([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y]?[0-9][0-9]?)|(([a-pr-uwyzA-PR-UWYZ][0-9][a-hjkstuwA-HJKSTUW])|([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y][0-9][abehmnprv-yABEHMNPRV-Y]))) {0,}[0-9][abd-hjlnp-uw-zABD-HJLNP-UW-Z]{2}))"

scot_hosp = scot_hosp %>% 
  mutate(Postcode = str_extract(Address, postcode_regex))

if (is.null(scot_hosp$Latitude)) {  # don't look up hospital coordinates multiple times
  ##
  ## get coordinates for postcodes
  ##
  if (!exists("postcodes")) postcodes = load_postcodes()
  
  # the ONS data truncates 7-character postcodes to remove spaces (e.g. CM99 1AB --> CM991AB); get rid of all spaces in both datasets to allow merging
  postcodes$Postcode2 = gsub(" ", "", postcodes$Postcode)
  scot_hosp$Postcode2 = gsub(" ", "", scot_hosp$Postcode)
  
  # merge
  scot_hosp = scot_hosp %>% 
    left_join(postcodes, by="Postcode2")
  
  # clean up postcode columns
  scot_hosp$Postcode2 = NULL  # don't need the truncated column anymore
  scot_hosp$Postcode.y = NULL
  scot_hosp = rename(scot_hosp, Postcode = Postcode.x)
  
  write_csv(scot_hosp, file.path(nhs.dir, "Scotland", "NHS Hospitals in Scotland.csv"))
}
