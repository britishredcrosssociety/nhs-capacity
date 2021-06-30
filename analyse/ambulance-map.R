# ---- load libs ----
library(tidyverse)
library(geographr)
library(sf)
library(lubridate)
library(viridis)

# ---- load data ----
ambulance <-
  readRDS("app/data/ambulance.rds")

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
 # ---- Prep data ----
 ambulance_cleaned <-
  ambulance %>% 
  filter(Category == "cat4") %>% 
  select(
    trust_code = `Trust Code`,
    mean_response_time = `Mean Response Time (min:sec)`
  ) %>% 
  mutate(mean_response_seconds = period_to_seconds(ms(mean_response_time))) %>% 
  mutate(
    response_mins = str_extract(mean_response_time, "^.{2}"),
    response_mins = as.double(response_mins)
  )

ambulance_points <-
  points_nhs_trusts %>% 
  right_join(ambulance_cleaned, by = c("nhs_trust_code" = "trust_code")) %>% 
  mutate(
    nhs_trust_name = str_to_title(nhs_trust_name),
    nhs_trust_name = str_replace(nhs_trust_name, "Nhs", "NHS")
  ) %>% 
  mutate(
    nhs_trust_name = str_remove_all(
      nhs_trust_name, "Service|Ambulance|NHS|Trust|Foundation|University"
      ),
    nhs_trust_name = str_squish(nhs_trust_name)
    ) %>% 
  mutate(
    label = str_c(nhs_trust_name, " \n", "Mean time: ", mean_response_time)
  )

# ---- Plot ----
boundaries_counties_ua %>% 
  filter(str_detect(county_ua_code, "^E")) %>% 
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
    name = expression(paste("Mean Cat4 \nResponse Time \n(mins)")),
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)
  ) +
  theme_map()
