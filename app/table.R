table_UI <- function(id) {
  dataTableOutput(NS(id, "nhs_table"))
}

table_server <- function(id, df, trust, dom_elements = "t") {
  stopifnot(is.reactive(trust))
  
  moduleServer(id, function(input, output, session) {
    output$nhs_table <- renderDataTable({
      datatable(
        df %>%
          # filter(`Trust Code` == trust()) %>%
          filter(if_any(contains("Code"), ~ .x == trust())) %>% 
          select(
            -contains("Name"),
            -contains("Code")
          ) %>%
          na.omit(),
        options = list(dom = dom_elements),
        rownames = FALSE
      )
    })
  })
}