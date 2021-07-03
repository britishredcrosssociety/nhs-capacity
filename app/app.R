# ---- Load libraries ----
library(shiny)
library(shiny.router)
library(shinyjs)
library(sf)
library(leaflet)
library(dplyr)
library(DT)
library(echarts4r)

source("charts.R")
source("table.R")
source("england-page.R")
source("wales-page.R")

# ---- Load data sets ----
# Map points
points_trusts <-
  read_sf("data/open_nhs_trusts_points.geojson")

points_wales <- 
  read_sf("data/wales.geojson")

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

# Wales data
wales_ae <- 
  readRDS("data/wales-ae.rds")

wales_ambulance <- 
  readRDS("data/wales-ambulance.rds")

wales_beds <- 
  readRDS("data/wales-beds.rds")

wales_cancer <- 
  readRDS("data/wales-cancer.rds")

wales_rtt <- 
  readRDS("data/wales-rtt.rds")

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

# ---- Router ----
router <- make_router(
  route("/", england_page),
  # route("/england", england_page),
  route("wales", wales_page)
)

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

  # Show map and indicators for selected nation
  router$ui,

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
  
  router$server(input, output, session)

  # Track which Trust has been selected
  selected_trust <- reactiveVal()

  # ---- Trust selection boxes for each nation ----
  # - England - 
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
  
  # - Wales - 
  observeEvent(input$selectbox_wales, {
    if (input$selectbox_wales == "") {
      selected_trust("7A1")
    } else {
      points_wales %>%
        filter(hb_name == input$selectbox_wales) %>%
        pull(hb_code) %>%
        selected_trust()
    }
    
    # Debug
    # print(selected_trust())
    
    # Show/hide indicators based on whether there's any data for the currently selected Health Board
    toggle(id = "ae_box",         condition = !wales_ae %>% filter(`Health Board Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "beds_box",       condition = !wales_beds %>% filter(`Health Board Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "cancer_box",     condition = !wales_cancer %>% filter(`Health Board Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "rtt_box",        condition = !wales_rtt %>% filter(`Health Board Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
    toggle(id = "ambulance_box",  condition = !wales_ambulance %>% filter(`Health Board Code` == selected_trust()) %>% pull(value) %>% is.na() %>% all())
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

  # ---- Maps ----
  # - England -
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

  # - Wales -
  output$map_wales <- renderLeaflet({
    leaflet() %>%
      # Centre on Wales; coordinates from https://nominatim.openstreetmap.org/ui/details.html?osmtype=R&osmid=58437&class=boundary
      setView(lat = 52.2928116, lng = -3.73893, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(
        data = points_wales,
        label = ~hb_name,
        icon = icons,
        layerId = ~hb_code
      )
  })
  
  # ---- A&E ----
  # - England -
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

  # - Wales -
  chart_server(
    id = "wales_ae_plot", 
    df = wales_ae,
    trust = selected_trust,
    wrangling_code = function(df)
      df %>% 
      filter(name == "Percentages") %>% 
      arrange(value) %>% 
      mutate(
        name = "Red calls",
        value = value / 100
      ), 
    label = "Percentage less than or equal to 4 hours", 
    axis_format = "percent"
  )
  
  table_server(
    "wales_ae_table", 
    df = wales_ae %>% 
      mutate(
        value = if_else(grepl("Percentages", name), round(value, digits = 1), round(value, digits = 0))
      ) %>%
      rename(Metric = name, Value = value),
    trust = selected_trust
  )
  
  # ---- Ambulance ----
  # - England -
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
  
  # - Wales -
  chart_server(
    id = "wales_ambulance_plot", 
    df = wales_ambulance,
    wrangling_code = function(df)
      df %>% 
      filter(grepl("%", name)) %>% 
      arrange(value) %>%
      mutate(
        name = "Red calls",
        value = value / 100
      ) %>% 
      na.omit(), 
    trust = selected_trust,
    label = "% ofresponses arriving within 8 minutes", 
    axis_format = "percent"
  )
  
  table_server(
    "wales_ambulance_table", 
    df = wales_ambulance %>% 
      mutate(
        value = if_else(grepl("%", name), round(value, digits = 1), round(value, digits = 0))
      ) %>%
      rename(Metric = name, Value = value),
    trust = selected_trust
  )

  # ---- Beds ----
  # - England -
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

  # - Wales -
  chart_server(
    id = "wales_beds_plot", 
    df = wales_beds,
    trust = selected_trust,
    wrangling_code = function(df)
      df %>% 
      arrange(value) %>%
      mutate(name = "% occupied") %>%
      na.omit(), 
    label = "Percentage of G&A Beds Occupied", 
    axis_format = "percent"
  )
  
  table_server(
    "wales_beds_table", 
    df = wales_beds %>% 
      mutate(
        value = if_else(grepl("%", name), round(value, digits = 3), round(value, digits = 0)),
        value = if_else(grepl("%", name), value * 100, value)
      ) %>%
      rename(Metric = name, Value = value),
    trust = selected_trust
  )
  
  # ---- Cancer wait times ----
  # - England -
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
  
  # - Wales -
  chart_server(
    id = "wales_cancer_plot", 
    df = wales_cancer,
    wrangling_code = function(df)
      df %>% 
      arrange(value) %>%
      mutate(
        name = "Within 62 days",
        value = value / 100
      ) %>%
      na.omit(), 
    trust = selected_trust,
    label = "Percentage Within Standard",
    axis_format = "percent"
  )
  
  table_server(
    "wales_cancer_table", 
    df = wales_cancer %>% 
      mutate(value = round(value, 1)) %>% 
      rename(Metric = name, Value = value),
    trust = selected_trust
  )
  
  # ---- Diagnostic wait times ----
  # - England -
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

  # ---- Consultant-led Outpatient Referrals ----
  # - England -
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

  # ---- RTT ----
  # - England -
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
  
  # - Wales -
  chart_server(
    id = "wales_rtt_plot", 
    df = wales_rtt,
    wrangling_code = function(df)
      df %>% 
      filter(grepl("%", name)) %>%
      arrange(value) %>%
      mutate(name = factor(name, levels = name)) %>%
      na.omit() %>%
      mutate(
        name = case_when(
          name == "% waiting 53+ weeks" ~ "53+ Weeks",
          name == "% waiting 18+ weeks" ~ "18+ Weeks"
        )
      ), 
    trust = selected_trust,
    label = "Percentage Waiting", 
    axis_format = "percent"
  )
  
  table_server(
    "wales_rtt_table", 
    df = wales_rtt %>% 
      mutate(
        value = if_else(grepl("%", name), round(value, digits = 3), round(value, digits = 0)),
        value = if_else(grepl("%", name), value * 100, value)
      ) %>% 
      rename(Metric = name, Value = value),
    trust = selected_trust
  )
}

# Run the application
shinyApp(ui = ui, server = server)