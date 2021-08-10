# ---- load libs ----
library(tidyverse)
library(geographr)
library(sf)
library(lubridate)
library(viridis)

# devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

# ---- load data ----
ambulance <-
  readRDS("app/data/ambulance.rds")

wales_ambo <- 
  read_csv("data/wales-ambo.csv")

# ---- plotting fun ----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "FiraCode-Retina", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      # Add labs elements
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        margin = margin(
          b = 0.2,
          t = 0.2,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(
          t = 0.2,
          b = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}

# ---- England Cat 4 ----
# - Prep data -
ambulance_cleaned <-
  ambulance |> 
  filter(Category == "cat4") |> 
  select(
    trust_code = `Trust Code`,
    mean_response_time = `Mean Response Time (min:sec)`
  ) |> 
  mutate(mean_response_seconds = period_to_seconds(ms(mean_response_time))) |> 
  mutate(
    response_mins = str_extract(mean_response_time, "^.{2}"),
    response_mins = as.double(response_mins)
  )

ambulance_points <-
  points_nhs_trusts |> 
  right_join(ambulance_cleaned, by = c("nhs_trust_code" = "trust_code")) |> 
  mutate(
    nhs_trust_name = str_to_title(nhs_trust_name),
    nhs_trust_name = str_replace(nhs_trust_name, "Nhs", "NHS")
  ) |> 
  mutate(
    nhs_trust_name = str_remove_all(
      nhs_trust_name, "Service|Ambulance|NHS|Trust|Foundation|University"
      ),
    nhs_trust_name = str_squish(nhs_trust_name)
    ) |> 
  mutate(
    label = str_c(nhs_trust_name, " \n", "Mean time: ", mean_response_time)
  )

# - Plot -
boundaries_counties_ua |> 
  filter(str_detect(county_ua_code, "^E")) |> 
  ggplot() +
  geom_sf(fill = NA, size = .2) +
  geom_sf(
    data = ambulance_points,
    mapping = aes(colour = response_mins),
    size = 12
  ) +
  geom_sf_label(
    data = ambulance_points,
    mapping = aes(label = label),
    nudge_y = .25,
    label.size = .4
    ) +
  scale_colour_viridis(
    direction = -1,
    begin = 0,
    end = .95,
    name = expression(paste("Mean Cat 4 \nResponse Time \n(mins)")),
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)
  ) +
  theme_map()

ggsave("charts/england-ambulance-cat-4.png", height = 275, width = 275, units = "mm")

# ---- England Cat 1 ----
# - Prep data -
ambulance_cleaned <-
  ambulance |> 
  filter(Category == "cat1") |> 
  select(
    trust_code = `Trust Code`,
    mean_response_time = `Mean Response Time (min:sec)`
  ) |> 
  mutate(mean_response_seconds = period_to_seconds(ms(mean_response_time))) |> 
  mutate(
    response_mins = str_extract(mean_response_time, "^.{2}"),
    response_mins = as.double(response_mins)
  )

ambulance_points <-
  points_nhs_trusts |> 
  right_join(ambulance_cleaned, by = c("nhs_trust_code" = "trust_code")) |> 
  mutate(
    nhs_trust_name = str_to_title(nhs_trust_name),
    nhs_trust_name = str_replace(nhs_trust_name, "Nhs", "NHS")
  ) |> 
  mutate(
    nhs_trust_name = str_remove_all(
      nhs_trust_name, "Service|Ambulance|NHS|Trust|Foundation|University"
    ),
    nhs_trust_name = str_squish(nhs_trust_name)
  ) |> 
  mutate(
    label = str_c(nhs_trust_name, " \n", "Mean time: ", mean_response_time)
  )

# - Plot -
boundaries_counties_ua |> 
  filter(str_detect(county_ua_code, "^E")) |> 
  ggplot() +
  geom_sf(fill = NA, size = .2) +
  geom_sf(
    data = ambulance_points,
    mapping = aes(colour = response_mins),
    size = 12
  ) +
  geom_sf_label_repel(
    data = ambulance_points,
    mapping = aes(label = label),
    nudge_y = .25,
    label.size = .4
  ) +
  scale_colour_viridis(
    direction = -1,
    begin = 0,
    end = .95,
    name = expression(paste("Mean Cat 1 \nResponse Time \n(mins)")),
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)
  ) +
  theme_map()

ggsave("charts/england-ambulance-cat-1.png", height = 250, width = 250, units = "mm")

# ---- Wales red ----
wales_ambo_points <- 
  points_wales_health_boards |> 
  right_join(wales_ambo, by = c("hb_code" = "HB_code")) |> 
  mutate(
    label = str_c(HB, " \n", "Calls arriving within 8 mins: ", round(`Red calls - % of emergency responses arriving at the scene within 8 minutes`, 1), "%")
  )

# - Plot -
boundaries_lad |> 
  filter(str_detect(lad_code, "^W")) |> 
  ggplot() +
  geom_sf(fill = NA, size = .2) +
  geom_sf(
    data = wales_ambo_points,
    mapping = aes(colour = `Red calls - % of emergency responses arriving at the scene within 8 minutes`),
    size = 12
  ) +
  geom_sf_label_repel(
    data = wales_ambo_points,
    mapping = aes(label = label),
    # nudge_y = .25,
    force = 100,
    label.size = .4
  ) +
  scale_colour_viridis(
    breaks = seq(54, 70, by = 2),
    labels = seq(54, 70, by = 2),
    direction = 1,
    begin = 0,
    end = .95,
    name = expression(paste("% of red calls arriving \n within 8 mins)")),
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = F)
  ) +
  theme_map()

ggsave("charts/wales-ambulance-red-cat.png", height = 175, width = 175, units = "mm")
