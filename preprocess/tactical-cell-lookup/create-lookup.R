library(tidyverse)
library(leaflet)
library(geographr)
library(sf)

boundaries_tc <-
  read_sf("preprocess/tactical-cell-lookup/tactical-cell-boundaries/Tactical_cells.shp")

boundaries_tc <-
  boundaries_tc |>
  st_transform(crs = 4326) |>
  st_make_valid(stp)

leaflet(
  options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = F)
) %>%
  setView(lat = 54.00366, lng = -2.547855, zoom = 7) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMapPane("boundaries_tc", zIndex = 430) |>
  addMapPane("boundaries_stp", zIndex = 440) |>
  addPolygons(
    data = boundaries_stp,
    fillColor = "#e3bf7a",
    weight = 0.7,
    opacity = 0.8,
    color = "black",
    dashArray = "2",
    fillOpacity = 0.1,
    options = pathOptions(pane = "boundaries_stp"),
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~stp_code,
    group = "boundaries_stp"
  ) |>
  addPolygons(
    data = boundaries_tc,
    fillColor = "#98c379",
    weight = 0.7,
    opacity = 1,
    color = "black",
    dashArray = "2",
    fillOpacity = 0.7,
    options = pathOptions(pane = "boundaries_tc"),
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~name,
    group = "boundaries_tc"
  ) |>
  addLayersControl(
    overlayGroups = c(
      "boundaries_stp",
      "boundaries_tc"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

# stp_tc_lookup <-
boundaries_stp |>
  st_drop_geometry() |>
  mutate(
    tactical_cell = case_when(
      stp_code == "E54000007" ~ "",
      stp_code == "E54000008" ~ "",
      stp_code == "E54000009" ~ "",
      stp_code == "E54000010" ~ "",
      stp_code == "E54000011" ~ "",
      stp_code == "E54000012" ~ "",
      stp_code == "E54000013" ~ "",
      stp_code == "E54000014" ~ "",
      stp_code == "E54000015" ~ "",
      stp_code == "E54000016" ~ "",
      stp_code == "E54000017" ~ "",
      stp_code == "E54000018" ~ "",
      stp_code == "E54000019" ~ "",
      stp_code == "E54000020" ~ "",
      stp_code == "E54000021" ~ "",
      stp_code == "E54000022" ~ "",
      stp_code == "E54000023" ~ "",
      stp_code == "E54000024" ~ "",
      stp_code == "E54000025" ~ "",
      stp_code == "E54000026" ~ "",
      stp_code == "E54000027" ~ "",
      stp_code == "E54000028" ~ "",
      stp_code == "E54000029" ~ "",
      stp_code == "E54000030" ~ "",
      stp_code == "E54000031" ~ "",
      stp_code == "E54000032" ~ "",
      stp_code == "E54000034" ~ "",
      stp_code == "E54000036" ~ "",
      stp_code == "E54000037" ~ "",
      stp_code == "E54000038" ~ "",
      stp_code == "E54000039" ~ "",
      stp_code == "E54000040" ~ "",
      stp_code == "E54000041" ~ "",
      stp_code == "E54000042" ~ "",
      stp_code == "E54000043" ~ "",
      stp_code == "E54000054" ~ "",
    )
  )