# ---- Load libs ----
library(tidyverse)
library(leaflet)
library(geographr)
library(sf)

# ---- Load BRC tactical cell boundaries for England ----
boundaries_tc <-
  read_sf("preprocess/uk/tactical-cell-lookup/tactical-cell-boundaries/Tactical_cells.shp")

# Align geometries
boundaries_tc <-
  boundaries_tc |>
  st_transform(crs = 4326) |>
  st_make_valid(stp)

# ---- Create leaflet map -----
# Inspect overlap between tactical cells and STP's
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

# Manually fill table
england_lookup <-
  boundaries_stp |>
  st_drop_geometry() |>
  mutate(
    tactical_cell = case_when(
      stp_code == "E54000007" ~ "North",
      stp_code == "E54000008" ~ "North",
      stp_code == "E54000009" ~ "North",
      stp_code == "E54000010" ~ "Central",
      stp_code == "E54000011" ~ "Central",
      stp_code == "E54000012" ~ "Central",
      stp_code == "E54000013" ~ "Central",
      stp_code == "E54000014" ~ "Central",
      stp_code == "E54000015" ~ "Central",
      stp_code == "E54000016" ~ "Central",
      stp_code == "E54000017" ~ "Central",
      stp_code == "E54000018" ~ "Central",
      stp_code == "E54000019" ~ "Central",
      stp_code == "E54000020" ~ "Central",
      stp_code == "E54000021" ~ "Central",
      stp_code == "E54000022" ~ "Central",
      stp_code == "E54000023" ~ "Central",
      stp_code == "E54000024" ~ "South East",
      stp_code == "E54000025" ~ "South East",
      stp_code == "E54000026" ~ "South East",
      stp_code == "E54000027" ~ "London",
      stp_code == "E54000028" ~ "London",
      stp_code == "E54000029" ~ "London",
      stp_code == "E54000030" ~ "London",
      stp_code == "E54000031" ~ "London",
      stp_code == "E54000032" ~ "South East",
      stp_code == "E54000034" ~ "South East",
      stp_code == "E54000036" ~ "South and the Channel Islands",
      stp_code == "E54000037" ~ "South and the Channel Islands",
      stp_code == "E54000038" ~ "South and the Channel Islands",
      stp_code == "E54000039" ~ "South and the Channel Islands",
      stp_code == "E54000040" ~ "South and the Channel Islands",
      stp_code == "E54000041" ~ "South and the Channel Islands",
      stp_code == "E54000042" ~ "South East",
      stp_code == "E54000043" ~ "South and the Channel Islands",
      stp_code == "E54000044" ~ "South and the Channel Islands",
      stp_code == "E54000048" ~ "North",
      stp_code == "E54000050" ~ "North",
      stp_code == "E54000051" ~ "North",
      stp_code == "E54000052" ~ "South East",
      stp_code == "E54000053" ~ "South East",
      stp_code == "E54000054" ~ "North"
    )
  ) |>
  select(geo_code = stp_code, tactical_cell)

# ---- Create other nation lookups ----
wales_lookup <-
  boundaries_lhb |>
  st_drop_geometry() |>
  mutate(tactical_cell = "Wales") |>
  select(geo_code = lhb_code, tactical_cell)

scotland_lookup <-
  boundaries_hb |>
  st_drop_geometry() |>
  mutate(tactical_cell = "Scotland") |>
  select(geo_code = hb_code, tactical_cell)

northern_ireland_lookup <-
  boundaries_trusts_ni |>
  st_drop_geometry() |>
  mutate(tactical_cell = "Northern Ireland and Isle of Man") |>
  select(geo_code = trust_code, tactical_cell)

# ---- Join lookups ----
tactical_cell_lookup <-
  bind_rows(
    england_lookup,
    wales_lookup,
    scotland_lookup,
    northern_ireland_lookup
  )

write_rds(tactical_cell_lookup, "preprocess/uk/tactical-cell-lookup/tactical_cell_lookup.rds")