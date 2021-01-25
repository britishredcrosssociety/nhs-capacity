# NHS Capacity <img src='figures/brc-logo.png' align="right" height ="65"/>

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)

## Navigation

- **`app/`** hosts a self-contained R Shiny app.
- **`data/`** contains `raw/` and `processed/` sub-folders.
- **`scrape/`** contains scripts to scrape raw data from the web and save them to `data/raw/`
- **`analyse/`** contains pre-processing scripts to generate data and insights. Takes data from `data/raw/` and exports to `data/processed/`.
- **`output/`** is a place for all non Shiny output (e.g., reports, plots, static files)

## Contributing
To contribute to this project, please follow [GitHub Flow](https://guides.github.com/introduction/flow/) when submitting changes.

> Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Getting help
If you encounter a clear bug, please file a minimal reproducible example in [issues](https://github.com/britishredcrosssociety/local-lockdown/issues).