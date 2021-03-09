# ---- Load libraries ----
library(shiny)
library(sf)
library(dplyr)

# ---- Load data sets ----
points_trusts <-
  read_sf("data/points_nhs_trusts.geojson")

ae <-
  readRDS("data/ae.rds")

beds_days <-
  readRDS("data/beds_days.rds")

beds_nights <-
  readRDS("data/beds_nights.rds")

cancer_wait_times <-
  readRDS("data/cancer_wait_times.rds")

diagnostic_wait_times <-
  readRDS("data/diagnostic_wait_times.rds")

outpatient_referrals <-
  readRDS("data/diagnostic_wait_times.rds")

rtt <-
  readRDS("data/referral_treatment_waiting_times.rds")

# ---- UI ----
ui <- navbarPage(

  # - Title -
  title = "NHS Capacity Analysis",

  # - Tab Panels -
  # Explore Home Page
  tabPanel(
    title = "Explore",
    
    fluidPage(
      
    )
    
  ),
  
  # User Guide
  tabPanel(
    "Guide"
  )
)

# ---- Server ----
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)
