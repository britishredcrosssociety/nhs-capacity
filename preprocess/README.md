## Purpose
The files in this folder preprocess raw NHS data from the web. Where web data can't be scraped, it is stored in the `preprocess/data` subfolder. Each indicator occupies an individual `.R` file with the output being saved into `/data` using `usethis::use_data()`. These data sets are then loaded into the shiny app using `pkgload::load_all(".")`.

## Metadata

| Nation | Indicator | Date | Source | License |
| --- | --- | --- | --- | --- |
| England | A&E | June 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| England | Ambulance Quality Indicators | June 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| England | Bed Occupancy (Day & Night) | Jan-March 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| England | Cancer Wait Times | May 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| England | Diagnostic Wait Times  | May 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| England | Outpatient Referrals | May 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-referrals/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| England | Referral to Treatment Waiting Times | May 2021 | [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| Northern Ireland | A&E | March 2021 | [Department of Health](https://www.health-ni.gov.uk/articles/emergency-care-waiting-times) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) |
| Northern Ireland | Reattendance | 2019-20 | [Department of Health](https://www.health-ni.gov.uk/) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) |
| Northern Ireland | Cancer Waiting Lists | March 2021 | [Department of Health](https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-cancer-waiting-times-january-march-2021) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) |
| Northern Ireland | Outpatient Waiting Times | March 2021 | [Department of Health](https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-outpatient-waiting-times-march-2021) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) |
| Northern Ireland | Inpatient Waiting Times | March 2021 | [Department of Health](https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-inpatient-and-day-case-waiting-times-march-2021) | [OGLv3](https://www.health-ni.gov.uk/crown-copyright) |
| Scotland | A&E | June 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written Permission Required](https://www.nhsperforms.scot/terms-conditions/copyright/) |
| Scotland | Bed Availability | December 2020 | [NHS Performs](https://www.nhsperforms.scot/) | [Written Permission Required](https://www.nhsperforms.scot/terms-conditions/copyright/) |
| Scotland | Cancer Waiting Times | March 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written Permission Required](https://www.nhsperforms.scot/terms-conditions/copyright/) |
| Scotland | Delayed Transfer of Care | April 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written Permission Required](https://www.nhsperforms.scot/terms-conditions/copyright/) |
| Scotland | Referral to Treatment Waiting Times | March 2021 | [NHS Performs](https://www.nhsperforms.scot/) | [Written Permission Required](https://www.nhsperforms.scot/terms-conditions/copyright/) |
| Wales | A&E | June 2021 | [StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Accident-and-Emergency) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| Wales | Bed Availability | August 2021 | [StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Activity/nhs-activity-and-capacity-during-the-coronavirus-pandemic/nhsbeds-by-date-localhealthboard) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| Wales | Ambulance Services | June 2021 |[StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Performance/Ambulance-Services) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) |
| Wales | Cancer Waiting Times | May 2021 | [StatsWales](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Cancer-Waiting-Times/Monthly/suspectedcancerpathwayclosedpathways-by-localhealthboard-tumoursite-agegroup-gender-measure-month) | [OGLv3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) | 