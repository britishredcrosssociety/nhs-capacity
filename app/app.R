# ---- Load libraries ----
library(shiny)
library(sf)
library(leaflet)

# ---- Load data sets ----
points_trusts <-
  read_sf("data/open_nhs_trusts_points.geojson")

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
ui <- fluidPage(

  # - Top bar with logos -
  fluidRow(
    tags$h4("Logos go here")
  ),

  # - Instructions -
  fluidRow(
    tags$h3("What is the capacity of Your Local NHS Trust?"),
    tags$p("by the British Red Cross, date."),
    tags$p("NHS Trusts are under pressure and are exceeding their capacity to
           cope. Enter your Trust in the box below, or select it on the map,
           to see how much pressure it is under.")
  ),

  # - Trust Search Box -
  fluidRow(
    tags$h3("Trust search box goes here")
  ),

  # - Map & Plots -
  fluidRow(

    # - Map -
    column(
      width = 4,
      leafletOutput("map", height = 800)
    ),

    # - Plots -
    column(
      width = 8,

      # - Row 1 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6
        ),

        # - Col 2 -
        column(
          width = 6
        )
      ),

      # - Row 2 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6
        ),

        # - Col 2 -
        column(
          width = 6
        )
      ),

      # - Row 3 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6
        ),

        # - Col 2 -
        column(
          width = 6
        )
      )
    ) # - Plots -
  ) # - Maps & Plots -
) # fluidPage

# ---- Server ----
server <- function(input, output) {
  
  # Map
  output$map <- renderLeaflet({
    leaflet(data = points_trusts) %>% 
      setView(lat = 54.00366, lng = -2.547855, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(popup = ~org_name, label = ~org_name)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
