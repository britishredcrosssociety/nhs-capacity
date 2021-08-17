# ---- Load libs ----
library(tidyverse)
library(usethis)

# ---- Load funs ----
source("https://github.com/britishredcrosssociety/resilience-index/raw/main/R/utils.R")

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

# ---- Load data ----
ae <- read_rds("preprocess/data/england_ae.rds")
ambulance <- read_rds("preprocess/data/england_ambulance_quality_indicators.rds")
beds <- read_rds("preprocess/data/england_bed_occupancy.rds")
cancer <- read_rds("preprocess/data/england_cancer_wait_times.rds")
diagnostic <- read_rds("preprocess/data/england_diagnostic_wait_times.rds")
outpatient <- read_rds("preprocess/data/england_outpatient_referrals.rds")
rtt <- read_rds("preprocess/data/england_referral_treatment_waiting_times.rds")
