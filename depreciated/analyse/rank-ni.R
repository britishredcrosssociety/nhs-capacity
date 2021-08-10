library(tidyverse)

# ---- Load funs ----
# 1 = best
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

# Return 1 if `x` is in the worst-performing quintile
count_if_worst <- function(x, q = 5) ifelse(!is.na(x) & x == q, 1, 0)

# ---- Load data ----
ae <- read_csv("data/ni-ae.csv")
cancer <- read_csv("data/ni-cancer.csv")
reattend <- read_csv("data/ni-reattendance.csv")
rtt_inpatient <- read_csv("data/ni-rtt-inpatients.csv")
rtt_outpatient <- read_csv("data/ni-rtt-outpatients.csv")

ni <- 
  ae |> 
  select(-Month) |> 
  
  left_join(cancer, by = "Trust") |> 
  left_join(reattend, by = "Trust") |> 
  left_join(
    rtt_inpatient |> select(-Date), 
    by = "Trust"
  ) |> 
  left_join(
    rtt_outpatient |> select(-Date), 
    by = "Trust"
  )

# ---- Find worst-performing Trusts across all metrics ----
# Bin each metric into quintiles and look for Trustss in the worst-performing quintile across multiple metrics
# There are only five Trusts in NI, so can just rank them rather than use quantise
ni_performance <- 
  ni |> 
  mutate(
    ae_bin = inverse_rank(`Percent Under 4 Hours`),
    cancer_bin = inverse_rank(`% treated within 62 days`),
    reattend_bin = inverse_rank(Reattend),
    rtt_in_bin = inverse_rank(`Inpatient and day case: % waiting > 52 weeks`),
    rtt_out_bin = inverse_rank(`Outpatient: % waiting > 52 weeks`)
  ) |> 
  
  # Calculate overall Trust performance, based on the quintiles
  rowwise() |>
  mutate(bin_sum = sum(c_across(ae_bin:rtt_out_bin), na.rm = TRUE)) |> 
  ungroup() |> 
  
  mutate(
    sum_of_5s = count_if_worst(ae_bin) +
      count_if_worst(cancer_bin) +
      count_if_worst(reattend_bin) +
      count_if_worst(rtt_in_bin) +
      count_if_worst(rtt_out_bin)
  ) |> 
  
  arrange(desc(sum_of_5s))

ni_performance |> 
  rename(
    `% reattendances within 7 days` = Reattend,
    `A&E performance (5 = worst performing)` = ae_bin,
    `Cancer waiting list performance (5 = worst performing)` = cancer_bin,
    `Inpatient and day case waiting list performance (5 = worst performing)` = rtt_in_bin,
    `Outpatient waiting list performance (5 = worst performing)` = rtt_out_bin
  ) |> 
  select(-bin_sum, -sum_of_5s) |> 
  
  write_csv(file = "data/nhs-performance-ni.csv")
