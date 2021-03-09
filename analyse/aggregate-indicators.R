# ---- Libraries ----
library(tidyverse)

# ---- Load data ----
file_path <- "data/processed/"

file_names <-
  list.files(file_path) %>% 
  .[str_detect(., "_provider")]

file_names %>%
  map(function(file_name){ # iterate through each file name
    assign(x = str_remove(file_name, ".csv"), # Remove file extension ".csv"
           value = read_csv(paste0(file_path, file_name)),
           envir = .GlobalEnv)
  })


