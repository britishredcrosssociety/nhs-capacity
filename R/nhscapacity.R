# ---- Load Libraries ----
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(ggiraph)
library(ggplot2)

# ---- Function that calls the app ----
nhsCapacity <- function() {

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
        width = 5,
        align = "center",
        leafletOutput("map", height = 900)
      ),

      # Plot
      column(
        width = 7,
        align = "center",
        plotOutput("plot", height = 900)
      )
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {

    # - Track user area selection -
    selected_area <- reactiveVal()

    # Track map click events and update reactives selectbox
    observeEvent(input$map_shape_click, {
      input$map_shape_click$id |>
        selected_area()

      selected_geo_name <-
        uk_shp |>
        st_drop_geometry() |>
        filter(geo_code == selected_area()) |>
        select(geo_name)

      updateSelectInput(
        session,
        "selectbox",
        selected = selected_geo_name
      )
    })

    # Track selectbox events
    observeEvent(input$selectbox,
      {
        uk_shp |>
          st_drop_geometry() |>
          filter(geo_name == input$selectbox) |>
          select(geo_code) |>
          pull() |>
          selected_area()
      },
      ignoreInit = TRUE
    )

    # Debug
    # observe({
    #   print(selected_area())
    # })

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
            color = "#5C747A",
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

    # - Plot -
    output$plot <- renderPlot({

      # Require user to validate input
      validate(
        need(
          !is.null(selected_area()),
          "Select an area to display some metrics here."
        )
      )

      selected_tactical_cell <-
        uk_long |>
        filter(geo_code == selected_area()) |>
        distinct(tactical_cell) |>
        pull(tactical_cell)

      # This is calculated to vertically center the labels in geom_text below
      num_tactical_cell_areas <-
        uk_long |>
        filter(tactical_cell == selected_tactical_cell) |>
        filter(grepl("rank$", variable)) |>
        distinct(geo_name) |>
        pull(geo_name) |>
        length()

      # Build lollipop plot
      uk_long |>
        filter(tactical_cell == selected_tactical_cell) |>
        filter(grepl("rank$", variable)) |>
        mutate(variable = as.factor(variable)) |>
        mutate(geo_name = reorder_within(geo_name, score, variable)) |>
        ggplot(
          aes(
            x = geo_name,
            y = score,
            colour = if_else(geo_code == selected_area(), "Red", "Blue")
          )
        ) +
        facet_wrap(vars(variable), scales = "free_y") +
        geom_segment(
          aes(x = geo_name, xend = geo_name, y = 0, yend = score),
          show.legend = FALSE
        ) +
        geom_point(
          size = 5,
          show.legend = FALSE
        ) +
        scale_colour_manual(
          values = c(Red = "#AD1220", Blue = "#475C74")
        ) +
        geom_rect(
          aes(xmin = -Inf, xmax = Inf, ymin = max(score) * .8, ymax = Inf),
          alpha = .025,
          fill = "#9CAAAE",
          color = NA,
          show.legend = FALSE
        ) +
        geom_rect(
          aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = max(score) * .2),
          alpha = .025,
          fill = "#475C74",
          color = NA,
          show.legend = FALSE
        ) +
        geom_text(
          aes(
            x = num_tactical_cell_areas / 2,
            y = max(score) * .9,
            label = "Worse Performance",
            angle = 270,
            hjust = "middle",
            vjust = "middle"
          ),
          show.legend = FALSE
        ) +
        geom_text(
          aes(
            x = num_tactical_cell_areas / 2,
            y = max(score) * .1,
            label = "Best Performance",
            angle = 270,
            hjust = "middle",
            vjust = "middle"
          ),
          show.legend = FALSE
        ) +
        coord_flip() +
        scale_x_reordered() +
        theme_minimal() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(x = NULL, y = NULL)
    })
  }
  shinyApp(ui, server)
}