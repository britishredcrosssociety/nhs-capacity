# ---- Load Libraries ----
library(shiny)
library(sf)
library(leaflet)
library(dplyr)

# ---- Function that calls the app ----
nhscapacity <- function() {

  # ---- UI ----
  ui <- fluidPage(

    # - Top bar with logos -
    fluidRow(
      column(
        width = 4,
        tags$div(
          style = "padding-top: 10px; padding-right: 10px;",
          img(src = "capacity-logo.jpg", width = 150)
        )
      ),
      column(
        width = 4,
        align = "center",
        tags$div(
          style = "padding-top: 10px;",
          tags$a(
            href = "https://redcross.org.uk",
            target = "_blank",
            img(src = "brc-team-logo.jpg", width = 400)
          ) # a
        ) # Div
      ), # Column
      column(
        width = 4,
        align = "right",
        tags$div(
          style = "padding-top: 15px; padding-right: 10px;",
          tags$a(
            href = "https://github.com/britishredcrosssociety/nhs-capacity",
            target = "_blank",
            icon("github", "fa-2x"),
            tags$style(".fa-github {color:#262626}")
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
          style = "padding-top: 12px; padding-bottom: 8px",
          "What is the Capacity of Your Local NHS Trust?"
        ),
        tags$p(
          style = "width:600px; padding-top: 12px; padding-bottom:12px",
          "Enter your Trust in the box below, or click it on the map, to explore
        the different pressures it is facing. Click on the 'Data' tabs above
        each plot to see more metrics. Use the toggle in the
        top-right corner of the map to view different domains of the",
          a(
            href = "https://healthindex.lcp.uk.com/",
            target = "_blank",
            "ONS Health Index."
          )
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