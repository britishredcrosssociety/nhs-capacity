library(shiny)

nhsCapacityApp <- function(...) {
  ui <- fluidPage(
    ...
  )
  server <- function(input, output, session) {
    ...
  }
  shinyApp(ui, server, ...)
}