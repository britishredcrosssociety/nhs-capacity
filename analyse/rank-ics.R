# ---- Load libs ----
library(tidyverse)
library(geographr)
library(sf)

# ---- Load funs ----
# 1 = best
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

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

# TODO:
# 1. Can Trust population sizes be added to normalise missing indicators?
# 2. Join/combine data
# 3. Lookup Trust -> ICS/STP
# 4. Find highest ranking (i.e., worst performing) Trusts for each health
#    data set.
# 5. Review calls to drop_na(), do they make sense given how ranking handles
#    NA values?