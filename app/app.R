# ---- Load libraries ----
library(shiny)
library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)

# ---- Load data sets ----
points_trusts <-
  read_sf("data/open_nhs_trusts_points.geojson")

ae <-
  readRDS("data/ae.rds")

beds <-
  readRDS("data/beds.rds")

cancer_wait_times <-
  readRDS("data/cancer_wait_times.rds")

diagnostic_wait_times <-
  readRDS("data/diagnostic_wait_times.rds")

outpatient_referrals <-
  readRDS("data/diagnostic_wait_times.rds")

rtt <-
  readRDS("data/referral_treatment_waiting_times.rds")

# ---- Create Markers ----
# Compatible markers: https://fontawesome.com/v4.7.0/icons/
icons <-
  awesomeIcons(
    icon = "h-square",
    lib = "fa",
    iconColor = "#FFFFFF",
    markerColor = "cadetblue"
  )
# ---- UI ----
ui <- fluidPage(

  # - Top bar with logos -
  fluidRow(
    column(
      width = 4,
      tags$div(
        style = "padding-top: 15px; padding-right: 10px;",
        icon("h-square", "fa-2x"),
        tags$style(".fa-h-square {color:#262626}")
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
  ), # fluidRow

  # - Instructions -
  fluidRow(
    column(width = 2),
    column(
      width = 8,
      align = "center",
      tags$h1("What is the Capacity of Your Local NHS Trust?"),
      tags$p(
        style = "font-size:12px;",
        "By Mike Page, Matt Thomas, Elle Gordon, & Freya Neason, 2021."
      ),
      tags$p(
        style = "width:520px; padding-top: 12px;",
        "NHS Trusts are under pressure and are exceeding their capacity to
        cope. Enter your Trust in the box below, or select it on the map,
        to explore the different pressures it is facing. Click on the data tabs
        above each plot to see more metrics."
      )
    ),
    column(width = 2)
  ),

  # - Trust Search Box -
  fluidRow(
    column(
      width = 12,
      align = "center",
      tags$h3("Trust search box goes here")
    )
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
          width = 6,

          h3("Accident and Emergency"),
          tabsetPanel(
            tabPanel("Plot", plotOutput("ae_plot", height = "200px")),
            tabPanel("Data", DTOutput("ae_table"))
          )
        ),

        # - Col 2 -
        column(
          width = 6,

          h3("Bed Occupancies (Day & Night)"),
          tabsetPanel(
            tabPanel("Plot", plotOutput("beds_plot", height = "200px")),
            tabPanel("Data", DTOutput("beds_table"))
          )
        )
      ),

      # - Row 2 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6,

          h3("Tabset Panel Name"),
          tabsetPanel(
            tabPanel("Plot"),
            tabPanel("Data")
          )
        ),

        # - Col 2 -
        column(
          width = 6,

          h3("Tabset Panel Name"),
          tabsetPanel(
            tabPanel("Plot"),
            tabPanel("Data")
          )
        )
      ),

      # - Row 3 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6,

          h3("Tabset Panel Name"),
          tabsetPanel(
            tabPanel("Plot"),
            tabPanel("Data")
          )
        ),

        # - Col 2 -
        column(
          width = 6,

          h3("Tabset Panel Name"),
          tabsetPanel(
            tabPanel("Plot"),
            tabPanel("Data")
          )
        )
      )
    ) # - Plots -
  ), # - Maps & Plots -

  fluidRow(
    column(
      width = 12,
      align = "center",
      tags$p(
        style = "font-size: 9px;",
        a(
          href = "https://github.com/britishredcrosssociety/nhs-capacity/blob/main/LICENSE",
          target = "_blank",
          "This work is licensed under GPL-3.0."
        )
      )
    )
  )
) # fluidPage

# ---- Server ----
server <- function(input, output) {

  # Debug
  # observe({
  #   print(input$map_marker_click$id)
  # })

  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 54.00366, lng = -2.547855, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(
        data = points_trusts,
        popup = ~org_name,
        label = ~org_name,
        icon = icons,
        layerId = ~org_code
      )
  })

  # Observe map click events
  selected_trust <- reactive({
    if (is.null(input$map_marker_click$id)) {
      return("RJZ")
    } else {
      return(input$map_marker_click$id)
    }
  })

  # A&E
  output$ae_plot <- renderPlot({
    ae %>%
      filter(`Trust Code` == selected_trust()) %>%
      filter(grepl("%", name)) %>%
      ggplot(aes(x = name, y = value)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = percent) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      )
  })

  output$ae_table <- renderDT({
    datatable(
      ae %>%
        filter(`Trust Code` == selected_trust()) %>%
        select(
          -`Trust Name`,
          -`Trust Code`,
          Metric = name,
          value
        ) %>%
        mutate(
          value = round(value, digits = 3),
          value = if_else(grepl("^%", Metric), value * 100, value)
        ),
      options = list(dom = "t")
    )
  })

  # Beds Days
  output$beds_plot <- renderPlot({
    beds %>%
      filter(`Trust Code` == selected_trust()) %>%
      filter(
        name == "% Total Day Beds Occupied" |
          name == "% Total Night Beds Occupied" |
          name == "% General Acute Night Beds Occupied" |
          name == "% General Acute Day Beds Occupied"
      ) %>%
      ggplot(aes(x = name, y = value)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = percent) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      )
  })

  output$beds_table <- renderDT({
    datatable(
      beds %>%
        filter(`Trust Code` == selected_trust()) %>%
        select(
          -`Trust Name`,
          -`Trust Code`,
          Metric = name,
          value
        ) %>%
        mutate(
          value = round(value, digits = 3),
          value = value * 100
        ),
      options = list(dom = "t")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# TODO:
# - Add titles to tabsetPanels
# - Find a method to sensibly handle missing values in ggplot
# - Theme the app using bslib in line with the BRC Design Library
# - Add White space around/plots maps by using padding or columns offsets
# - Change license link colour and update license
