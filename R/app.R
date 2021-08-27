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
            placeholder = "Select an NHS area",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    ),

    # - Map & Plot -
    fluidRow(

      # Map
      column(
        width = 6,
        align = "center",
        leafletOutput("map", height = 600)
      ),

      # Plot
      column(
        width = 6,
        align = "center"
      )
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {

    # # - Track user area selection -
    # selected_area <- reactiveVal("")

    # observeEvent(input$map_shape_click, {
    #   input$map_shape_click$id |>
    #   selected_area()

    #   # retrieve the lad name for the polygon chosen on map
    #   selected_in_dropdown <- imd_with_boundaries |>
    #   st_drop_geometry() |>
    #   filter(lad_code == selected_area()) |>
    #   select(lad_name)

    #   # update the name selected on the selectInput so reflects that chosen on map
    #   updateSelectInput(session, "selectbox", selected = selected_in_dropdown)
    # })

    # observeEvent(input$selectbox,
    #   {

    #     # retrieve the lad_code for the lad_name selected on the dropdown
    #     imd_with_boundaries |>
    #     st_drop_geometry() |>
    #     filter(lad_name == input$selectbox) |>
    #     select(lad_code) |>
    #     pull() |>
    #     selected_area()
    #   },
    #   ignoreInit = TRUE
    # )

    # - Map -
    output$map <-
      renderLeaflet({
        leaflet() |>
        setView(lat = 52.75, lng = -2.0, zoom = 6) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          data = uk_shp,
          layerId = ~geo_code,
          weight = 0.7,
          opacity = 0.5,
          # color = "#bf4aee",
          dashArray = "0.1",
          fillOpacity = 0.4,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = uk_shp$geo_name
        )
      })
  }
  shinyApp(ui, server)
}