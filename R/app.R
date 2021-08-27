# ---- Load Libraries ----
library(shiny)
library(sf)
library(leaflet)
library(dplyr)

# ---- Function that calls the app ----
nhscapacity <- function() {

  # ---- UI ----
  ui <- fluidPage(

    # - Set CSS -
    includeCSS("inst/www/styles.css"),

    # - Top bar with logos -

    # Capacity logo
    fluidRow(
      column(
        width = 4,
        tags$div(
          class = "capacity-logo",
          img(src = "www/capacity-logo.jpg", width = 150)
        )
      ),

      # BRC logo
      column(
        width = 4,
        align = "center",
        tags$div(
          class = "brc-logo",
          tags$a(
            href = "https://redcross.org.uk",
            target = "_blank",
            img(src = "www/brc-team-logo.jpg", width = 400)
          )
        )
      ),

      # GitHub logo
      column(
        width = 4,
        align = "right",
        tags$div(
          class = "github-logo",
          tags$a(
            href = "https://github.com/britishredcrosssociety/nhs-capacity",
            target = "_blank",
            icon("github", "fa-2x")
          )
        )
      )
    ),

    # - Instructions -
    fluidRow(
      column(width = 2),
      column(
        width = 8,
        align = "center",
        tags$h1(
          "What is the Capacity of Your Local NHS Area?"
        ),
        tags$p(
          "Enter your area in the box below, or click it on the map to explore
          the pressures it is facing."
        )
      ),
      column(width = 2)
    ),

    # - Trust Search Box -
    fluidRow(
      column(
        width = 12,
        align = "center",
        selectizeInput(
          "selectbox",
          label = NULL,
          choices = sort(unique(uk_shp$geo_name)),
          options = list(
            placeholder = "Select an NHS Trust",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {

  }
  shinyApp(ui, server)
}