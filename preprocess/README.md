## Purpose
The files in this folder preprocess NHS data to be fed into the Shiny app.

## Structure
- Each devolved nation occupies a single subfolder
- In each subfolder, each indicator occupies an individual `.R` file. Cleaned data is saved into `/prepcoess/data`
- Where web data can't be scraped, it is stored in the `preprocess/data/raw` subfolder
- Each subfolder also contains a `performance.R` script for aggregating all indicators into a single table. 
- Each aggregated nation table are then combined into UK wide data sets in the `/uk` subfolder. These UK data sets are then fed into the shiny app using `usethis::use_data()`. These data sets are then loaded into the shiny app using `pkgload::load_all(".")`.

## Metadata
| Nation | Indicator | Date | Source | License | Last Updated | Reflected in Dashboard |
| --- | --- | --- | --- | --- | --- | --- |
| England | A&E | November 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| England | Ambulance Quality Indicators | Novmeber 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| England | Bed Occupancy (Day & Night) | July-September 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| England | Cancer Wait Times | October 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| England | Diagnostic Wait Times  | October 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| England | Outpatient Referrals | October 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-referrals/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| England | Referral to Treatment Waiting Times | October 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| England | IAPT | Q1 2021-22 | [NHS England](https://digital.nhs.uk/data-and-information/publications/statistical/psychological-therapies-report-on-the-use-of-iapt-services/june-2021-final-including-reports-on-the-iapt-pilots-and-quarter-1-data-2021-22) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 10.12.21 | :heavy_check_mark: |
| Northern Ireland | A&E | March 2021 | [Department of Health](https://www.health-ni.gov.uk/articles/emergency-care-waiting-times) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) | | |
| Northern Ireland | Reattendance | 2019-20 | [Department of Health](https://www.health-ni.gov.uk/) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) | | |
| Northern Ireland | Cancer Waiting Lists | March 2021 | [Department of Health](https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-cancer-waiting-times-january-march-2021) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) | | |
| Northern Ireland | Outpatient Waiting Times | March 2021 | [Department of Health](https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-outpatient-waiting-times-march-2021) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) | | |
| Northern Ireland | Inpatient Waiting Times | March 2021 | [Department of Health](https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-inpatient-and-day-case-waiting-times-march-2021) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) | | |
| Scotland | A&E | June 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written permission received](https://www.nhsperforms.scot/terms-conditions/copyright/) | | |
| Scotland | Bed Availability | December 2020 | [NHS Performs](https://www.nhsperforms.scot/) | [Written permission received](https://www.nhsperforms.scot/terms-conditions/copyright/) | | |
| Scotland | Cancer Waiting Times | March 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written permission received](https://www.nhsperforms.scot/terms-conditions/copyright/) | | |
| Scotland | Delayed Transfer of Care | April 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written permission received](https://www.nhsperforms.scot/terms-conditions/copyright/) | | |
| Scotland | Referral to Treatment Waiting Times | March 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written permission received](https://www.nhsperforms.scot/terms-conditions/copyright/) | | |
| Wales | A&E | October 2021 | [StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Accident-and-Emergency) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 13.12.21 | :x: |
| Wales | Bed Availability | December 2021 | [StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Activity/nhs-activity-and-capacity-during-the-coronavirus-pandemic/nhsbeds-by-date-localhealthboard) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 13.12.21 | :x: |
| Wales | Ambulance Services | June 2021 |[StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Performance/Ambulance-Services) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | | |
| Wales | Cancer Waiting Times | May 2021 | [StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Cancer-Waiting-Times/Monthly/suspectedcancerpathwayclosedpathways-by-localhealthboard-tumoursite-agegroup-gender-measure-month) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | | | 
| Wales | Referral to Treatment Waiting Times | May 2021 | [StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Referral-to-Treatment/patientpathwayswaitingtostarttreatment-by-month-groupedweeks-treatmentfunction) |[OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | | |