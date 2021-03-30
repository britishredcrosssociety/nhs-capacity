# ---- Load libraries ----
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(DT)
library(echarts4r)

# ---- Load data sets ----
# Map points
points_trusts <-
  read_sf("data/open_nhs_trusts_points.geojson")

# Indicators
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

# Health Index
health_index <-
  readRDS("data/health_index.rds")

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
    theme = '{"color":["#5C747A","#193351","#6A9EAA"]}',
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
      a {
          color: #5C747A;
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
        style = "padding-top: 12px; padding-bottom: 8px",
        "What is the Capacity of Your Local NHS Trust?"
      ),
      tags$p(
        style = "width:520px; padding-top: 12px; padding-bottom:12px",
        "NHS Trusts are facing substantial pressure. Enter your Trust in the box
        below, or click it on the map, to explore the different pressures it is 
        facing. Click on the 'Data' tabs above each plot to see more metrics."
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
      leafletOutput("map", height = 1015)
    ),

    # - Plots -
    column(
      width = 8,

      # - Row 1 -
      fluidRow(

        # - Col 1 -
        column(
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Accident and Emergency"),
            h6("Latest data: Jan 2021"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("ae_plot", height = "200px")),
              tabPanel("Data", DTOutput("ae_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Bed Occupancies (Day & Night)"),
            h6("Latest data: Dec 2020"),
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
          align = "center",
          tags$div(
            id = "card",
            h4("Cancer Wait Times"),
            h6("Latest data: Dec 2020"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("cancer_plot", height = "200px")),
              tabPanel("Data", DTOutput("cancer_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Diagnostic Wait Times"),
            h6("Latest data: Dec 2020"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("diagnostic_plot", height = "200px")),
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
          align = "center",
          tags$div(
            id = "card",
            h4("Consultant-led Outpatient Referrals"),
            h6("Latest data: Dec 2020"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("outpatient_plot", height = "200px")),
              tabPanel("Data", DTOutput("outpatient_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Consultant-led Referral to Treatment Waiting Times"),
            h6("Latest data: Dec 2020"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("rtt_plot", height = "200px")),
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
server <- function(input, output, session) {

  # Debug
  # s_trust <- reactiveVal()
  #
  # observeEvent(input$selectbox, {
  #   if(input$selectbox == ""){
  #     s_trust("RJZ")
  #   } else {
  #     points_trusts %>%
  #       filter(org_name == input$selectbox) %>%
  #       pull(org_code) %>%
  #       s_trust()
  #   }
  # })
  #
  # observeEvent(input$map_marker_click$id, {
  #   if(is.null(input$map_marker_click$id)){
  #     s_trust("RJZ")
  #   } else {
  #     input$map_marker_click$id %>%
  #       s_trust()
  #   }
  # })
  #
  # observe({
  #   print(s_trust())
  # })


  # Track which Trust has been selected
  selected_trust <- reactiveVal()

  observeEvent(input$selectbox, {
    if (input$selectbox == "") {
      selected_trust("RJZ")
    } else {
      points_trusts %>%
        filter(org_name == input$selectbox) %>%
        pull(org_code) %>%
        selected_trust()
    }
  })

  observeEvent(input$map_marker_click$id, {
    if (is.null(input$map_marker_click$id)) {
      selected_trust("RJZ")
    } else {
      input$map_marker_click$id %>%
        selected_trust()

      clicked_trust <- points_trusts %>%
        filter(org_code == input$map_marker_click$id) %>%
        pull(org_name)

      updateSelectizeInput(session, "selectbox", selected = clicked_trust)
    }
  })

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
      ) %>% 
      addPolygons(
        data = health_index,
        group = "Health Index Overall",
        fillColor = ~colorQuantile("YlOrRd", overall_health_index)(overall_health_index),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      ) %>% 
      addPolygons(
        data = health_index,
        group = "Healthy Lives",
        fillColor = ~colorQuantile("YlOrRd", healthy_lives)(healthy_lives),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      ) %>% 
      addPolygons(
        data = health_index,
        group = "Healthy Places",
        fillColor = ~colorQuantile("YlOrRd", healthy_places)(healthy_places),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      ) %>% 
      addPolygons(
        data = health_index,
        group = "Healthy People",
        fillColor = ~colorQuantile("YlOrRd", healthy_people)(healthy_people),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      )  %>% 
      addLayersControl(
    baseGroups = c(
      "Health Index Overall",
      "Healthy Lives",
      "Healthy Places", 
      "Healthy People"
      ),
    options = layersControlOptions(collapsed = TRUE)
  )
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
          left = 145,
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
          left = 145,
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
      filter(
        name == "62 Days" |
          name == "31 Days" |
          name == "2 Week Wait"
      ) %>%
      na.omit()

    if (nrow(cancer_temp) != 0) {
      cancer_temp %>%
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
        e_axis_labels(x = "Percentage Within Standard") %>%
        e_theme("brc_theme") %>%
        e_grid(
          left = 145,
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
  output$diagnostic_plot <- renderEcharts4r({
    diagnostic_temp <-
      diagnostic_wait_times %>%
      filter(`Trust Code` == selected_trust()) %>%
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      na.omit() %>%
      mutate(
        name = case_when(
          name == "Waiting 13+ weeks" ~ "13+ Weeks",
          name == "Waiting 6+ weeks" ~ "6+ Weeks",
          name == "Waiting List Total" ~ "Total List"
        )
      )

    if (nrow(diagnostic_temp) != 0) {
      diagnostic_temp %>%
        e_charts(name) %>%
        e_bar(value, itemStyle = list(opacity = .6)) %>%
        e_flip_coords() %>%
        e_legend(FALSE) %>%
        e_tooltip(trigger = "item") %>%
        e_x_axis(
          nameLocation = "middle",
          nameTextStyle = list(padding = 20)
        ) %>%
        e_axis_labels(x = "No. Waiting") %>%
        e_theme("brc_theme") %>%
        e_grid(
          left = 145,
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

  output$diagnostic_table <- renderDT({
    datatable(
      diagnostic_wait_times %>%
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

  # Consultant-led Outpatient Referrals
  output$outpatient_plot <- renderEcharts4r({
    outpatient_temp <-
      outpatient_referrals %>%
      filter(`Trust Code` == selected_trust()) %>%
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      na.omit() %>%
      mutate(
        name = case_when(
          name == "GP Referrals Made (All)" ~ "GP (All)",
          name == "Other Referrals Made (All)" ~ "Other (All)",
          name == "GP Referrals Made (Specific Acute)" ~ "GP (Specific Acute)",
          name == "Other Referrals Made (Specific Acute)" ~ "Other (Specific Acute)"
        )
      )

    if (nrow(outpatient_temp) != 0) {
      outpatient_temp %>%
        e_charts(name) %>%
        e_bar(value, itemStyle = list(opacity = .6)) %>%
        e_flip_coords() %>%
        e_legend(FALSE) %>%
        e_tooltip(trigger = "item") %>%
        e_x_axis(
          nameLocation = "middle",
          nameTextStyle = list(padding = 20)
        ) %>%
        e_axis_labels(x = "No. Referrals Made") %>%
        e_theme("brc_theme") %>%
        e_grid(
          left = 145,
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

  output$outpatient_table <- renderDT({
    datatable(
      outpatient_referrals %>%
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

  # rtt
  output$rtt_plot <- renderEcharts4r({
    rtt_temp <-
      rtt_long_form %>%
      filter(`Trust Code` == selected_trust()) %>%
      filter(`Referral Treatment Type` != "New RTT Periods - All Patients") %>%
      filter(grepl("%", name)) %>%
      group_by(`Referral Treatment Type`) %>%
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      ungroup() %>%
      na.omit() %>%
      mutate(
        `Referral Treatment Type` = case_when(
          `Referral Treatment Type` == "Completed Pathways For Admitted Patients" ~ "Admitted Completed",
          `Referral Treatment Type` == "Completed Pathways For Non-Admitted Patients" ~ "Non-Admitted Completed",
          `Referral Treatment Type` == "Incomplete Pathways" ~ "Incomplete",
          `Referral Treatment Type` == "Incomplete Pathways with DTA" ~ "Incomplete with DTA"
        ),
        name = case_when(
          name == "% Waiting 52+ Weeks" ~ "52+ Weeks",
          name == "% Waiting 18+ Weeks" ~ "18+ Weeks"
        )
      )


    if (nrow(rtt_temp) != 0) {
      rtt_temp %>%
        group_by(name) %>%
        e_charts(`Referral Treatment Type`) %>%
        e_bar(value, itemStyle = list(opacity = .6)) %>%
        e_flip_coords() %>%
        e_legend(
          orient = "vertical",
          left = "right",
          top = "center"
        ) %>%
        e_tooltip(trigger = "item") %>%
        e_x_axis(
          formatter = e_axis_formatter("percent"),
          nameLocation = "middle",
          nameTextStyle = list(padding = 20)
        ) %>%
        e_axis_labels(x = "Percentage Waiting on Pathway") %>%
        e_theme("brc_theme") %>%
        e_grid(
          left = 145,
          top = 20,
          bottom = 60,
          right = 105
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

  output$rtt_table <- renderDT({
    datatable(
      rtt %>%
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
}

# Run the application
shinyApp(ui = ui, server = server)

# BUGS:
# - Bug with Beds plot.
#   Test on 'University Hopsitals Dorset NHS Foundation Trust', code 'R0D'. Run
#   the debug observer and notice how the plot doesn't update to blank, even
#   though the table does.
# - Bug with Cancer plot. Test on 'Solent NHS Trust', code '1RC'
# - Bug with Diagnostic plot, code '1RA'
# - Bug with Consultant-led Outpatient Referrals