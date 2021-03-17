# ---- Load libraries ----
library(shiny)
library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(echarts4r)

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
  readRDS("data/outpatients_referrals.rds")

rtt <-
  readRDS("data/referral_treatment_waiting_times.rds")

rtt_long_form <-
  readRDS("data/referral_treatment_waiting_times_long_form.rds")

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

  # Use colours from BRC style guide:
  # - https://design-system.redcross.org.uk/styles/colours/
  e_theme_register(
    theme = '{"color":["#AD1220","#5C747A","#193351","#6A9EAA"]}',
    name = "brc_theme"
  ),

  # CSS Styles
  tags$head(
    tags$style(
      HTML("
      #card {
          box-shadow: 2px 2px 5px grey;
          padding: 10px 20px 10px 20px;
          margin: 0px 0px 20px 0px;
      }
      #map {
          box-shadow: 2px 2px 5px grey;
      }
      #footer {
          background-color: #262626;
          height: 165px;
      }
      ")
    )
  ),

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
  ), # fluidRow

  # - Instructions -
  fluidRow(
    column(width = 2),
    column(
      width = 8,
      align = "center",
      tags$h1(
        style = "padding-top: 12px;",
        "What is the Capacity of Your Local NHS Trust?"
      ),
      tags$p(
        style = "font-size:12px;",
        "By Mike Page, Matt Thomas, Elle Gordon, & Freya Neason, 2021."
      ),
      tags$p(
        style = "width:520px; padding-top: 12px; padding-bottom:12px",
        "NHS Trusts are under pressure and are exceeding their capacity to
        cope. Enter your Trust in the box below, or click it on the map,
        to explore the different pressures it is facing. Click on the 'Data' tabs
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
      selectizeInput(
        "selectbox",
        label = NULL,
        choices = sort(points_trusts$org_name),
        options = list(
          placeholder = "Select an NHS Trust",
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    )
  ),

  # - Map & Plots -
  fluidRow(

    # - Map -
    column(
      width = 4,
      leafletOutput("map", height = 945)
    ),

    # - Plots -
    column(
      width = 8,

      # - Row 1 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6,
          tags$div(
            id = "card",
            h4("Accident and Emergency"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("ae_plot", height = "200px")),
              tabPanel("Data", DTOutput("ae_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          width = 6,
          tags$div(
            id = "card",
            h4("Bed Occupancies (Day & Night)"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("beds_plot", height = "200px")),
              tabPanel("Data", DTOutput("beds_table"))
            )
          )
        )
      ),

      # - Row 2 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6,
          tags$div(
            id = "card",
            h4("Cancer Wait Times"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("cancer_plot", height = "200px")),
              tabPanel("Data", DTOutput("cancer_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          width = 6,
          tags$div(
            id = "card",
            h4("Diagnostic Wait Times"),
            tabsetPanel(
              tabPanel("Plot", plotOutput("diagnostic_plot", height = "200px")),
              tabPanel("Data", DTOutput("diagnostic_table"))
            )
          )
        )
      ),

      # - Row 3 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6,
          tags$div(
            id = "card",
            h4("Consultant-led Outpatient Referrals"),
            tabsetPanel(
              tabPanel("Plot", plotOutput("outpatient_plot", height = "200px")),
              tabPanel("Data", DTOutput("outpatient_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          width = 6,
          tags$div(
            id = "card",
            h4("Consultant-led Referral to Treatment Waiting Times"),
            tabsetPanel(
              tabPanel("Plot", plotOutput("rtt_plot", height = "200px")),
              tabPanel("Data", DTOutput("rtt_table"))
            )
          )
        )
      )
    ) # - Plots -
  ), # - Maps & Plots -

  fluidRow(
    id = "footer",
    column(
      width = 12,
      align = "center",
      br(),
      tags$p(
        style = "font-size: 12px; ",
        a(
          style = "color: #FFFFFF",
          href = "https://github.com/britishredcrosssociety/nhs-capacity/blob/main/LICENSE",
          target = "_blank",
          "This work is licensed under GPL-3.0."
        )
      ),
      tags$div(
        img(src = "footer.jpg", width = 1000)
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

  # observe({
  #   if(input$selectbox == "" & is.null(input$map_marker_click$id)) {
  #     print("RJZ")
  #   } else if(input$selectbox != "") {
  #     points_trusts %>%
  #       filter(org_name == input$selectbox) %>%
  #       pull(org_code) %>%
  #       print()
  #   } else if(!is.null(input$map_marker_click$id)) {
  #     input$map_marker_click$id %>%
  #       print()
  #   }
  # })

  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 54.00366, lng = -2.547855, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(
        data = points_trusts,
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
  output$ae_plot <- renderEcharts4r({
    ae_temp <-
      ae %>%
      filter(`Trust Code` == selected_trust()) %>%
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      na.omit() %>%
      filter(grepl("%", name)) %>% 
      mutate(
        name = case_when(
          name == "% Type 1 <= 4 hours" ~ "Type 1",
          name == "% Type 2 <= 4 hours" ~ "Type 2",
          name == "% Type 3 <= 4 hours" ~ "Type 3",
          name == "% Total <= 4 hours" ~ "Total"
        )
      )

    if (nrow(ae_temp) != 0) {
      ae_temp %>%
        e_charts(name) %>%
        e_bar(value, itemStyle = list(opacity = .6)) %>%
        e_flip_coords() %>%
        e_legend(FALSE) %>%
        e_tooltip(trigger = "item") %>%
        e_x_axis(
          formatter = e_axis_formatter("percent"),
          nameLocation = "middle",
          nameTextStyle = list(padding = 20)
          ) %>% 
        e_axis_labels(x = "Percentage less than or equal to 4 hours") %>% 
        e_theme("brc_theme") %>%
        e_grid(
          left = 50,
          top = 20,
          bottom = 60
          )
    } else {
      e_charts(data = NULL) %>%
        e_draft(
          text = "Unfortunately, this data doesn't exist!",
          size = "30px",
          color = "#5C747A"
        )
    }
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
        ) %>% 
        na.omit(),
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  # Beds
  output$beds_plot <- renderEcharts4r({
    beds_temp <- 
      beds %>% 
      filter(`Trust Code` == selected_trust()) %>% 
      filter(
        name == "% Total Day Beds Occupied" |
          name == "% Total Night Beds Occupied" |
          name == "% General Acute Night Beds Occupied" |
          name == "% General Acute Day Beds Occupied"
      ) %>% 
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>% 
      na.omit() %>% 
      mutate(
        name = case_when(
          name == "% Total Day Beds Occupied" ~ "Total Day",
          name == "% Total Night Beds Occupied" ~ "Total Night",
          name == "% General Acute Night Beds Occupied" ~ "General Acute Night",
          name == "% General Acute Day Beds Occupied" ~ "General Acute Day"
        )
      )
    
    if (nrow(beds_temp) != 0) {
      beds_temp %>%
        e_charts(name) %>%
        e_bar(value, itemStyle = list(opacity = .6)) %>%
        e_flip_coords() %>%
        e_legend(FALSE) %>%
        e_tooltip(trigger = "item") %>%
        e_x_axis(
          formatter = e_axis_formatter("percent"),
          nameLocation = "middle",
          nameTextStyle = list(padding = 20)
        ) %>% 
        e_axis_labels(x = "Percentage of Beds Occupied") %>% 
        e_theme("brc_theme") %>%
        e_grid(
          left = 120,
          top = 20,
          bottom = 60
        )
    } else {
      e_charts(data = NULL) %>%
        e_draft(
          text = "Unfortunately, this data doesn't exist!",
          size = "30px",
          color = "#5C747A"
        )
    }
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
        ) %>% 
        na.omit(),
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  # Cancer wait times
  output$cancer_plot <- renderEcharts4r({
    cancer_temp <-
      cancer_wait_times %>% 
      filter(`Trust Code` == selected_trust()) %>% 
      mutate(value = `Within Standard` / `Total Treated`) %>% 
      rename(name = Standard) %>% 
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      na.omit()
    
    if (nrow(cancer_temp) != 0) {
      cancer_temp %>%
        e_charts(name) %>%
        e_bar(value, itemStyle = list(opacity = .6)) %>%
        e_flip_coords() %>%
        e_legend(FALSE) %>%
        e_tooltip(trigger = "item") %>%
        e_x_axis(formatter = e_axis_formatter("percent"), nameLocation = "middle") %>%
        e_axis_labels(x = "Within Standard") %>% 
        e_theme("brc_theme") %>%
        e_grid(
          left = 120,
          top = 10,
          bottom = 10
          )
    } else {
      e_charts(data = NULL) %>%
        e_draft(
          text = "Unfortunately, this data doesn't exist!",
          size = "30px",
          color = "#5C747A"
        )
    }
  })

  output$cancer_table <- renderDT({
    datatable(
      cancer_wait_times %>%
        filter(`Trust Code` == selected_trust()) %>%
        select(
          -`Trust Name`,
          -`Trust Code`
        ) %>% 
        na.omit(),
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  # Diagnostic wait times
  output$diagnostic_plot <- renderPlot({
    diagnostic_wait_times %>%
      filter(`Trust Code` == selected_trust()) %>%
      ggplot(aes(x = name, y = value)) +
      geom_segment(aes(x = name, xend = name, y = 0, yend = value)) +
      geom_point(size = 5, colour = "#406574") +
      coord_flip() +
      theme_minimal() +
      labs(
        x = NULL,
        y = "No. People"
      )
  })

  output$diagnostic_table <- renderDT({
    datatable(
      diagnostic_wait_times %>%
        filter(`Trust Code` == selected_trust()) %>%
        select(
          -`Trust Name`,
          -`Trust Code`
        ),
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  # Consultant-led Outpatient Referrals
  output$outpatient_plot <- renderPlot({
    outpatient_referrals %>%
      filter(`Trust Code` == selected_trust()) %>%
      ggplot(aes(x = name, y = value)) +
      geom_segment(aes(x = name, xend = name, y = 0, yend = value)) +
      geom_point(size = 5, colour = "#406574") +
      coord_flip() +
      theme_minimal() +
      labs(
        x = NULL,
        y = "No. People"
      )
  })

  output$outpatient_table <- renderDT({
    datatable(
      outpatient_referrals %>%
        filter(`Trust Code` == selected_trust()) %>%
        select(
          -`Trust Name`,
          -`Trust Code`
        ),
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  # rtt
  output$rtt_plot <- renderPlot({
    rtt_long_form %>%
      filter(`Trust Code` == selected_trust()) %>%
      filter(grepl("%", name)) %>%
      ggplot(aes(x = `Referral Treatment Type`, y = value, colour = name)) +
      geom_linerange(
        aes(x = `Referral Treatment Type`, ymin = 0, ymax = value, colour = name),
        position = position_dodge(width = 1)
      ) +
      geom_point(position = position_dodge(width = 1), size = 3) +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = percent) +
      labs(
        x = NULL,
        y = NULL
      ) +
      scale_colour_manual(values = c("#475C74", "#9CAAAE"))
  })

  output$rtt_table <- renderDT({
    datatable(
      rtt %>%
        filter(`Trust Code` == selected_trust()) %>%
        select(
          -`Trust Name`,
          -`Trust Code`
        ),
      options = list(dom = "t"),
      rownames = FALSE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# TODO:
# - Sort out cancer plot.
# - Sort out other plots.
# - Give plots different colour themes to differentiate them.
# - Sort out Leaflet / Searchbox logic. Test with the observer.
# - Add Data set dates (last updated/available) & Sort out card titles (center & style them)
# - Remove scales and ggplot libraries be removed if using echarts4r
# - should the tabsetPanels be turned to pills with colours style?
# - Bug with Beds plot.
#   Test on 'University Hopsitals Dorset NHS Foundation Trust', code 'R0D'. Run
#   the debug observer and notice how the plot doesn't update to blank, even
#   though the table does.
# - Bug with Cancer plot. Test on 'Solent NHS Trust', code '1RC'
# - Deploy to shinyapps.io /nhs-capacity
# - Should the plots add a comparison to the mean scores / distributions for
#   all the other Trusts (normalised by population size or using percentages)?
# - Should ambulance trust data be added, with another card below the
#   map?
# - Should historial data be used?
# - Update the indicators with latest month?