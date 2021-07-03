source("charts.R")
source("table.R")

england_page <- div(
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
  ) # - Maps & Plots -
)