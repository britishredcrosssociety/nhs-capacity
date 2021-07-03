chart_UI <- function(id) {
  echarts4rOutput(NS(id, "nhs_chart"), height = "200px")
}

chart_server <- function(id, df, trust, wrangling_code, label = "", axis_format = "decimal") {
  moduleServer(id, function(input, output, session) {
    output$nhs_chart <- renderEcharts4r({
      df <- 
        df %>% 
        filter(if_any(contains("Code"), ~ .x == trust())) %>% 
        # filter(`Trust Code` == trust()) %>%
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
