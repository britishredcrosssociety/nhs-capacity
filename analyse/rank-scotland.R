library(tidyverse)

source("https://github.com/britishredcrosssociety/resilience-index/raw/main/R/utils.R")  # for quantise()

# ---- Load funs ----
# 1 = best
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

# ---- Load data ----
scotland <- read_csv("data/scotland-hospital-indicators.csv")

# ---- Find worst-performing Health Boards across all metrics ----
# Bin each metric into quintiles and look for HBs in the worst-performing quintile across multiple metrics
scotland_performance <- 
  scotland %>% 
  filter(`NHS Board` != "National") %>% 
  
  # Pick worst performance stats in each Health Board
  group_by(`NHS Board`) %>% 
  summarise(
    `Percentage seen within 4 hours` = min(`Percentage seen within 4 hours`, na.rm = TRUE),
    `Average number of available staffed beds` = min(`Average number of available staffed beds`, na.rm = TRUE),
    `Average number of days waited from receipt of an urgent referral with suspicion of cancer to first cancer treatment (62 day standard)` = max(`Average number of days waited from receipt of an urgent referral with suspicion of cancer to first cancer treatment (62 day standard)`, na.rm = TRUE),
    `Bed days occupied by delayed discharge patients` = max(`Bed days occupied by delayed discharge patients`, na.rm = TRUE),
    `Percentage seen within 18 weeks` = min(`Percentage seen within 18 weeks`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 

  # Calculate quintiles
  mutate(
    ae_bin = quantise(`Percentage seen within 4 hours`, num_quantiles = 5, highest_quantile_worst = FALSE),
    beds_bin = quantise(`Average number of available staffed beds`, num_quantiles = 5, highest_quantile_worst = FALSE),
    cancer_bin = quantise(`Average number of days waited from receipt of an urgent referral with suspicion of cancer to first cancer treatment (62 day standard)`, num_quantiles = 5, highest_quantile_worst = TRUE),
    dtoc_bin = quantise(`Bed days occupied by delayed discharge patients`, num_quantiles = 5, highest_quantile_worst = TRUE),
    rtt_bin = quantise(`Percentage seen within 18 weeks`, num_quantiles = 5, highest_quantile_worst = FALSE)
  ) %>% 

  # Calculate overall performance, based on the quintiles
  rowwise() %>%
  mutate(bin_sum = sum(c_across(ae_bin:rtt_bin), na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(
    sum_of_5s = count_if_worst(ae_bin) +
      count_if_worst(beds_bin) +
      count_if_worst(cancer_bin) +
      count_if_worst(dtoc_bin) +
      count_if_worst(rtt_bin)
  ) %>% 
  
  arrange(desc(sum_of_5s))

scotland_performance
