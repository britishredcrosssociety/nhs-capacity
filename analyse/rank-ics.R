# ---- Load libs ----
library(tidyverse)
library(geographr)
library(sf)

source("https://github.com/britishredcrosssociety/resilience-index/raw/main/R/utils.R")  # for quantise()

# ---- Load funs ----
# 1 = best
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

# ---- Load open Trusts ----
open_trusts <-
  read_sf("data/points_nhs_trusts.geojson") %>%
  as_tibble() %>%
  filter(status == "open") %>%
  select(
    org_name,
    org_code
  )

# ---- Load data ----
ae <-
  readRDS("app/data/ae.rds")

ambulance <-
  readRDS("app/data/ambulance.rds")

beds <-
  readRDS("app/data/beds.rds")

cancer_wait_times <-
  readRDS("app/data/cancer_wait_times.rds")

# Read preformatted as contains relative measures needed below
diagnostic_wait_times <-
  read_csv("data/nhs_diagnostic_waiting_times.csv")

outpatient_referrals <-
  readRDS("app/data/outpatients_referrals.rds")

rtt <-
  readRDS("app/data/referral_treatment_waiting_times_long_form.rds")

# ---- Analyse data ----
# Create summary metrics (mean rank) across each health data set
# Use relative measures
# Higher rank (1) = better performing
ae_rank <-
  ae %>% 
  filter(str_detect(name, "^%")) %>% 
  drop_na(value) %>% 
  group_by(name) %>% 
  mutate(rank = inverse_rank(value)) %>% 
  ungroup() %>% 
  select(trust_code = `Trust Code`, rank) %>% 
  group_by(trust_code) %>% 
  summarise(ae_rank = mean(rank)) %>% 
  arrange(ae_rank)

ambulance_rank <-
  ambulance %>% 
  select(
    trust_code = `Trust Code`,
    cat = Category,
    mean_time = `Mean Response Time (min:sec)`
    ) %>% 
  drop_na(mean_time) %>% 
  group_by(cat) %>% 
  mutate(rank = rank(mean_time)) %>% 
  ungroup() %>% 
  group_by(trust_code) %>% 
  summarise(ambo_rank = mean(rank)) %>% 
  arrange(ambo_rank)

beds_rank <-
  beds %>% 
  filter(str_detect(name, "^%")) %>% 
  drop_na(value) %>% 
  group_by(name) %>% 
  mutate(rank = rank(value)) %>% 
  ungroup() %>% 
  select(trust_code = `Trust Code`, rank) %>% 
  group_by(trust_code) %>% 
  summarise(beds_rank = mean(rank)) %>% 
  arrange(beds_rank)

# Cancer wait times - can these be analysed as there is no relative measure?
# Can Trust population sizes be obtained to normalise the data?

diagnostic_rank <-
  diagnostic_wait_times %>% 
  filter(org_code %in% open_trusts$org_code) %>% 
  select(trust_code = org_code, starts_with("per")) %>% 
  pivot_longer(
    cols = !starts_with("Trust")
  ) %>% 
  drop_na(value) %>%
  group_by(name) %>% 
  mutate(rank = rank(value)) %>% 
  ungroup() %>% 
  group_by(trust_code) %>% 
  summarise(diag_rank = mean(rank)) %>% 
  arrange(diag_rank)

# Outpatient referrals - can these be analysed as there is no relative measure?
# Can Trust population sizes be obtained to normalise the data?

rtt_rank <-
  rtt %>% 
  filter(str_detect(name, "^%")) %>% 
  select(
    trust_code = `Trust Code`,
    type = `Referral Treatment Type`,
    name,
    value
    ) %>% 
  drop_na(value) %>% 
  group_by(type, name) %>% 
  mutate(rank = rank(value)) %>% 
  ungroup() %>% 
  group_by(trust_code) %>% 
  summarise(rtt_rank = mean(rank)) %>% 
  arrange(rtt_rank)

# ---- Find worst-performing Trusts across all metrics ----
# Bin each metric into quintiles and look for Trusts in the worst-performing quintile across multiple metrics
ae_binned <- 
  ae %>% 
  filter(str_detect(name, "^% Total")) %>% 
  drop_na(value) %>% 
  
  group_by(name) %>% 
  mutate(bin = quantise(value, num_quantiles = 5, highest_quantile_worst = FALSE)) %>% 
  ungroup() %>% 
  
  select(trust_code = `Trust Code`, ae_bin = bin)
  
beds_binned <- 
  beds %>% 
  filter(str_detect(name, "^% Total Day")) %>% 
  drop_na(value) %>% 
  
  group_by(name) %>% 
  mutate(bin = quantise(value, num_quantiles = 5)) %>% 
  ungroup() %>% 
  
  select(trust_code = `Trust Code`, beds_bin = bin)

cancer_binned <- 
  cancer_wait_times %>% 
  mutate(value = Breaches / `Total Treated`) %>%
  rename(name = Standard) %>%
  
  filter(name == "62 Days") %>% 
  drop_na(value) %>% 
  
  group_by(name) %>% 
  mutate(bin = quantise(value, num_quantiles = 5)) %>% 
  ungroup() %>% 
  
  select(trust_code = `Trust Code`, cancer_bin = bin)

diagnostic_wait_times

rtt_binned <- 
  rtt %>% 
  filter(
    `Referral Treatment Type` == "Incomplete Pathways" &
      str_detect(name, "^Waiting 52+")
  ) %>% 
  
  drop_na(value) %>% 
  
  group_by(name) %>% 
  mutate(bin = quantise(value, num_quantiles = 5)) %>% 
  ungroup() %>% 
  
  select(trust_code = `Trust Code`, rtt_bin = bin)

# Calculate overall Trust performance, based on the quintiles
trust_performance <- 
  open_trusts %>% 
  rename(trust_name = org_name, trust_code = org_code) %>% 
  
  left_join(ae_binned, by = "trust_code") %>% 
  left_join(beds_binned, by = "trust_code") %>% 
  left_join(cancer_binned, by = "trust_code") %>% 
  left_join(rtt_binned, by = "trust_code") %>% 
  
  rowwise() %>%
  mutate(bin_sum = sum(c_across(ae_bin:rtt_bin), na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(
    sum_of_5s = count_if_worst(ae_bin) +
                count_if_worst(beds_bin) +
                count_if_worst(cancer_bin) +
                count_if_worst(rtt_bin)
  ) %>% 
  
  arrange(desc(sum_of_5s))

# Lookup ICS/STP for each Trust
stp_names <- read_sf("https://opendata.arcgis.com/datasets/08d5070b4f324560aeef857e26701f77_0.geojson") %>% 
  st_drop_geometry()

# List STPs/ICSs containing the worst-performing Trusts (with quintiles of 5 on three or more performance indicators)
trust_performance %>% 
  left_join(geographr::lookup_trust_stp, by = c("trust_code" = "nhs_trust_code")) %>% 
  left_join(stp_names, by = c("stp_code" = "STP20CDH")) %>% 
  
  filter(sum_of_5s >= 3) %>% 
  
  select(STP20NM) %>% 
  distinct() %>% 
  arrange(STP20NM)



# TODO:
# 1. Can Trust population sizes be added to normalise missing indicators?
# 2. Join/combine data
# 3. Lookup Trust -> ICS/STP
# 4. Find highest ranking (i.e., worst performing) Trusts for each health
#    data set.
# 5. Review calls to drop_na(), do they make sense given how ranking handles
#    NA values?


