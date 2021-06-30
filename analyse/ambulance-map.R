# ---- load libs ----
library(tidyverse)
library(geographr)
library(sf)
library(lubridate)

# ---- load data ----
ambulance <-
  readRDS("app/data/ambulance.rds")

# ---- plotting fun ----
map_theme <- function(...) {
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
  mutate(mean_response_seconds = period_to_seconds(ms(mean_response_time)))

ambulance_points <-
  points_nhs_trusts %>% 
  right_join(ambulance_cleaned, by = c("nhs_trust_code" = "trust_code")) %>% 
  mutate(
    nhs_trust_name = str_to_title(nhs_trust_name),
    nhs_trust_name = str_replace(nhs_trust_name, "Nhs", "NHS")
  )

# ---- Plot ----
ggplot(ambulance_points) +
  geom_sf(mapping = aes(fill = mean_response_seconds))
