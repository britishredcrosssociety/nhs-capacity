# ---- Load libraries ----
library(shiny)
library(shinyjs)
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

ambulance <-
  readRDS("data/ambulance.rds")

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

# ---- Modules for data tables ----
table_UI <- function(id) {
  dataTableOutput(NS(id, "england_table"))
}

table_server <- function(id, df, trust, dom_elements = "t") {
  stopifnot(is.reactive(trust))
  
  moduleServer(id, function(input, output, session) {
    output$england_table <- renderDataTable({
      datatable(
        df %>%
          filter(`Trust Code` == trust()) %>%
          select(
            -`Trust Name`,
            -`Trust Code`
          ) %>%
          na.omit(),
        options = list(dom = dom_elements),
        rownames = FALSE
      )
    })
  })
}

# ---- Modules for charts ----
chart_UI <- function(id) {
  echarts4rOutput(NS(id, "england_chart"), height = "200px")
}

chart_server <- function(id, df, trust, wrangling_code, label = "", axis_format = "decimal") {
  moduleServer(id, function(input, output, session) {
    output$england_chart <- renderEcharts4r({
      df <- 
        df %>% 
        filter(`Trust Code` == trust()) %>%
        wrangling_code() %>% 
        na.omit()
      
      if (nrow(df) != 0) {
        df %>%
          e_charts(name) %>%
          e_bar(value, itemStyle = list(opacity = .6)) %>%
          e_flip_coords() %>%
          e_legend(FALSE) %>%
          e_tooltip(trigger = "item") %>%
          e_x_axis(
            formatter = e_axis_formatter(axis_format),
            nameLocation = "middle",
            nameTextStyle = list(padding = 20)
          ) %>%
          e_axis_labels(x = label) %>%
          e_theme("brc_theme") %>%
          e_grid(
            left = 145,
            top = 20,
            bottom = 60
          )
      } else {
        tibble(x = NA) %>%
          e_charts(x) %>%
          e_draft(
            text = "This data doesn't exist for this trust.",
            size = "15px",
            color = "#5C747A"
          )
      }
    })
  })
}

# ---- UI ----
ui <- fluidPage(
  useShinyjs(),

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
          box-shadow: 0px 0px 3px grey;
          border-radius: 5px;
          padding: 10px 20px 10px 20px;
          margin: 0px 0px 20px 0px;
      }
      #map {
          box-shadow: 0px 0px 3px grey;
          border-radius: 5px;
          margin: 0px 0px 20px 0px;
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

    # - Map and single plot underneath map -
    column(
      width = 4,

      # - Map -
      fluidRow(

        # Wrapping in column(), again, provides padding to the left of the map
        column(
          width = 12,
          leafletOutput("map", height = 670)
        )
      )
    ),

    # - Grid of plots -
    column(
      width = 8,

      # - Row 1 -
      fluidRow(

        # - Col 1 -
        column(
          id = "ae_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Accident and Emergency"),
            h6("Latest data: Apr 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("ae_plot")),
              tabPanel("Data", table_UI("ae_table"))
              
            )
          )
        ),

        # - Col 2 -
        column(
          id = "beds_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Bed Occupancies (Day & Night)"),
            h6("Latest data: Jan-Mar 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("beds_plot")),
              tabPanel("Data", table_UI("beds_table"))
            )
          )
        ),

        # - Col 1 -
        column(
          id = "cancer_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Cancer Wait Times"),
            h6("Latest data: Mar 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("cancer_plot")),
              tabPanel("Data", table_UI("cancer_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          id = "diagnostic_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Diagnostic Wait Times"),
            h6("Latest data: Mar 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("diagnostic_plot")),
              tabPanel("Data", table_UI("diagnostic_table"))
            )
          )
        ),
        
        # - Col 1 -
        column(
          id = "outpatient_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Consultant-led Outpatient Referrals"),
            h6("Latest data: Mar 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("outpatient_plot")),
              tabPanel("Data", table_UI("outpatient_table"))
            )
          )
        ),

        # - Col 2 -
        column(
          id = "rtt_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Consultant-led Referral to Treatment Waiting Times"),
            h6("Latest data: Mar 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("rtt_plot")),
              tabPanel("Data", table_UI("rtt_table"))
            )
          )
        ),
        
        column(
          id = "ambulance_box",
          width = 12,
          align = "center",
          tags$div(
            id = "card",
            h4("Ambulance Response Times"),
            h6("Latest data: Feb 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("ambulance_plot")),
              tabPanel("Data", table_UI("ambulance_table"))
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
        style = "font-size: 12px; color: #FFFFFF",
        "Content is distributed under ",
        a(
          style = "color: #FFFFFF",
          href = "https://github.com/britishredcrosssociety/nhs-capacity/blob/main/LICENSE",
          target = "_blank",
          "these licenses."
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
    
    # Debug
    # print(selected_trust())

    # Show/hide indicators based on whether there's any data for the currently selected Trust
    toggle(id = "ae_box",         condition = !ae %>% filter(`Trust Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "beds_box",       condition = !beds %>% filter(`Trust Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "cancer_box",     condition = !cancer_wait_times %>% filter(`Trust Code` == selected_trust()) %>% pull(Standard) %>% is.na())
    toggle(id = "diagnostic_box", condition = !diagnostic_wait_times %>% filter(`Trust Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "outpatient_box", condition = !outpatient_referrals %>% filter(`Trust Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "rtt_box",        condition = !rtt_long_form %>% filter(`Trust Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "ambulance_box",  condition = !ambulance %>% filter(`Trust Code` == selected_trust()) %>% pull(Category) %>% is.na())
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

  # Debug
  # observe({
  #   print(selected_trust())
  # })

  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 52.75, lng = -2.0, zoom = 6) %>%
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
        fillColor = ~ colorQuantile("magma", overall_health_index, n = 5)(overall_health_index),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      ) %>%
      addPolygons(
        data = health_index,
        group = "Healthy Lives",
        fillColor = ~ colorQuantile("magma", healthy_lives, n = 5)(healthy_lives),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      ) %>%
      addPolygons(
        data = health_index,
        group = "Healthy Places",
        fillColor = ~ colorQuantile("magma", healthy_places, n = 5)(healthy_places),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      ) %>%
      addPolygons(
        data = health_index,
        group = "Healthy People",
        fillColor = ~ colorQuantile("magma", healthy_people, n = 5)(healthy_people),
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.4
      ) %>%
      addLegend(
        colors = c("#000004", "#51127C", "#B6367A", "#FB8861", "#FCFDBF"),
        labels = c("Least healthy", "", "", "", "Most Healthy"),
        position = "bottomleft"
      ) %>%
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
  chart_server(
    id = "ae_plot", 
    df = ae,
    trust = selected_trust,
    wrangling_code = function(df)
      df %>% 
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      filter(grepl("%", name)) %>%
      mutate(
        name = case_when(
          name == "% Type 1 <= 4 hours" ~ "Type 1",
          name == "% Type 2 <= 4 hours" ~ "Type 2",
          name == "% Type 3 <= 4 hours" ~ "Type 3",
          name == "% Total <= 4 hours" ~ "Total"
        )
      ), 
    label = "Percentage less than or equal to 4 hours", 
    axis_format = "percent"
  )
  
  table_server(
    "ae_table", 
    df = ae %>% 
      mutate(
        value = round(value, digits = 3),
        value = if_else(grepl("^%", name), value * 100, value)
      ) %>% 
      rename(Metric = name, Value = value),
    trust = selected_trust
  )

  # Ambulance
  chart_server(
    id = "ambulance_plot", 
    df = ambulance,
    wrangling_code = function(df)
      df %>% 
      arrange(`Total Response Time (h)`) %>%
      mutate(Category = factor(Category, levels = Category)) %>%
      na.omit() %>% 
      rename(name = Category, value = `Total Response Time (h)`), 
    trust = selected_trust,
    label = "Total Response Time (h)"
  )
  
  table_server(
    "ambulance_table", 
    df = ambulance,
    trust = selected_trust
  )

  # Beds
  chart_server(
    id = "beds_plot", 
    df = beds,
    trust = selected_trust,
    wrangling_code = function(df)
      df %>% 
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
      ), 
    label = "Percentage of Beds Occupied", 
    axis_format = "percent"
  )

  table_server(
    "beds_table", 
    df = beds %>% 
      mutate(
        value = if_else(grepl("%", name), round(value, digits = 3), round(value, digits = 0)),
        value = if_else(grepl("%", name), value * 100, value)
      ) %>%
      rename(Metric = name, Value = value),
    trust = selected_trust,
    dom_elements = "tp"
  )

  # Cancer wait times
  chart_server(
    id = "cancer_plot", 
    df = cancer_wait_times,
    wrangling_code = function(df)
      df %>% 
      mutate(value = `Within Standard` / `Total Treated`) %>%
      rename(name = Standard) %>%
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      filter(
        name == "62 Days" |
          name == "31 Days" |
          name == "2 Week Wait"
      ), 
    trust = selected_trust,
    label = "Percentage Within Standard",
    axis_format = "percent"
  )
  
  table_server(
    "cancer_table", 
    df = cancer_wait_times,
    trust = selected_trust
  )
  
  # Diagnostic wait times
  chart_server(
    id = "diagnostic_plot", 
    df = diagnostic_wait_times,
    wrangling_code = function(df)
      df %>% 
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      mutate(
        name = case_when(
          name == "Waiting 13+ weeks" ~ "13+ Weeks",
          name == "Waiting 6+ weeks" ~ "6+ Weeks",
          name == "Waiting List Total" ~ "Total List"
        )
      ), 
    trust = selected_trust,
    label = "No. Waiting"
  )
  
  table_server(
    "diagnostic_table", 
    df = diagnostic_wait_times,
    trust = selected_trust
  )

  # Consultant-led Outpatient Referrals
  chart_server(
    id = "outpatient_plot", 
    df = outpatient_referrals,
    wrangling_code = function(df)
      df %>% 
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      mutate(
        name = case_when(
          name == "GP Referrals Made (All)" ~ "GP (All)",
          name == "Other Referrals Made (All)" ~ "Other (All)",
          name == "GP Referrals Made (Specific Acute)" ~ "GP (Specific Acute)",
          name == "Other Referrals Made (Specific Acute)" ~ "Other (Specific Acute)"
        )
      ), 
    trust = selected_trust,
    label = "No. Referrals Made"
  )
  
  table_server(
    "outpatient_table", 
    df = outpatient_referrals,
    trust = selected_trust
  )

  # rtt
  chart_server(
    id = "rtt_plot", 
    df = rtt_long_form,
    wrangling_code = function(df)
      df %>% 
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
      ), 
    trust = selected_trust,
    label = "Percentage Waiting on Pathway", 
    axis_format = "percent"
  )
  
  table_server(
    "rtt_table", 
    df = rtt,
    trust = selected_trust
  )
}

# Run the application
shinyApp(ui = ui, server = server)