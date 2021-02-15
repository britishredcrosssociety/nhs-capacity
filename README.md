# NHS Capacity <img src='figures/brc-logo.png' align="right" height ="65"/>

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)

## Navigation

- **`app/`** hosts a self-contained R Shiny app.
- **`data/`** contains `raw/` and `processed/` sub-folders.
- **`analyse/`** contains scripts to generate data and insights. Takes data from `data/raw/` and the web and exports to `data/processed/`. Exports other output to `output/`
- **`output/`** is a place for all non Shiny output (e.g., reports, plots, static files)

## Data sets

Data set | Geographies | Date | Source | raw  
--- | --- | --- | --- | ---
A&E Attendances and Emergency Admissions | Provider (NHS Trusts, NHS Foundation Trusts and Independent Sector Organisations) & STP | Jan 2021 | https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2020-21/ | https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/January-2021-AE-by-provider-O64J2.xls  
Ambulance Quality Indicators | Provider (NHS Trusts, NHS Foundation Trusts and Independent Sector Organisations) | Jan 20201 | https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/ | https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/AmbSYS-for-Jan-2021.xlsx  
Bed Occupancy Night | NHS organisations | Q2 2020-21 | https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-overnight/ | https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx  
Bed Occupancy Day | NHS organisations | Q2 2020-2021 | https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-day-only/ | https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Day-Only-Web_File-Final-DE5WC.xlsx  
DToC | Provider | Feb 2020 (paused) | https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/delayed-transfers-of-care-data-2019-20/ | https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Trust-Type-B-February-2020-4W5PA.xls  
Inpatients (elective) & Outpatients | Provider | 2019-2020 Q4 | https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/quarterly-hospital-activity/qar-data/ | https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/05/QAR-PROV-Web-1920-Q4-aIu8F.xls  
Monthly Diagnostics | Provider | Dec 2020 | https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2020-21/ | https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Monthly-Diagnostics-Web-File-Provider-December-2020_C9B31.xls  
Care Home Beds | LA | Feb 2021 | https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory | https://www.cqc.org.uk/sites/default/files/HSCA_Active_Locations_1_February_2021.xlsx  

## Contributing
To contribute to this project, please follow [GitHub Flow](https://guides.github.com/introduction/flow/) when submitting changes.

> Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Getting help
If you encounter a clear bug, please file a minimal reproducible example in [issues](https://github.com/britishredcrosssociety/local-lockdown/issues).