source("charts.R")
source("table.R")

wales_page <- div(
  # - Trust Search Box -
  fluidRow(
    column(width = 2),
    column(
      width = 4,
      align = "center",
      
      selectizeInput(
        "selectbox_nation_wales",
        label = NULL,
        choices = c("England", "Wales", "Scotland", "Northern Ireland"),
        selected = "Wales",
        options = list(
          placeholder = "Select a nation"
        )
      )
    ),
    
    column(
      width = 4,
      align = "center",
      
      selectizeInput(
        "selectbox_wales",
        label = NULL,
        choices = sort(points_wales$hb_name),
        options = list(
          placeholder = "Select a Health Board",
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    column(width = 2)
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
          leafletOutput("map_wales", height = 670)
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
          id = "wales_ae_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Accident and Emergency"),
            h6("Latest data: Apr 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("wales_ae_plot")),
              tabPanel("Data", table_UI("wales_ae_table"))
              
            )
          )
        ),
        
        # - Col 2 -
        column(
          id = "wales_beds_box",
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Bed Occupancy"),
            h6("Latest data: Jan-Mar 2021"),
            tabsetPanel(
              tabPanel("Plot", chart_UI("wales_beds_plot")),
              tabPanel("Data", table_UI("wales_beds_table"))
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
              tabPanel("Plot", chart_UI("wales_cancer_plot")),
              tabPanel("Data", table_UI("wales_cancer_table"))
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
              tabPanel("Plot", chart_UI("wales_rtt_plot")),
              tabPanel("Data", table_UI("wales_rtt_table"))
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
              tabPanel("Plot", chart_UI("wales_ambulance_plot")),
              tabPanel("Data", table_UI("wales_ambulance_table"))
            )
          )
        )
      )
    ) # - Plots -
  ) # - Maps & Plots -
)