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