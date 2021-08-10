# ---- load libs ----
library(tidyverse)
library(geographr)
library(sf)
library(lubridate)
library(viridis)
library(patchwork)

# ---- Load funs ----
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

quantise <-
  function(vec,
           num_quantiles = 10,
           highest_quantile_worst = TRUE) {
    if (length(unique(vec)) <= 1) {
      stop("The vector cannot be quantised as there is only one unique value.")
    }
    
    quantile_breaks <-
      classInt::classIntervals(
        vec,
        num_quantiles,
        style = "quantile"
      )
    
    quantiles <-
      as.integer(
        cut(
          vec,
          breaks = quantile_breaks$brks,
          include.lowest = TRUE
        )
      )
    
    if (!highest_quantile_worst) {
      max_quant <- max(quantiles, na.rm = TRUE)
      quantiles <- (max_quant + 1) - quantiles
    }
    
    if (
      !(
        tibble(quantiles = quantiles) |>
        count(quantiles) |>
        mutate(
          equal_bins = if_else(
            n >= (length(vec) / num_quantiles) - 1 &
            n <= (length(vec) / num_quantiles) + 1,
            TRUE,
            FALSE
          )
        ) |>
        pull(equal_bins) |>
        all()
      )
      
    ) {
      warning("Quantiles are not in equal bins.")
    }
    
    return(quantiles)
  }

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

# ---- Load data ----
eng_hi_shp <- read_rds("app/data/health_index.rds")

sco_hi <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/scotland/index-unweighted.csv")
  
sco_hi_shp <-
  boundaries_lad |> 
  filter(str_detect(lad_code, "^S")) |> 
  left_join(sco_hi, by = "lad_code")

# ---- Quantise Eng HI ----
eng_hi_shp <- 
  eng_hi_shp |> 
  mutate(
    healthy_lives_rank = inverse_rank(healthy_lives),
    healthy_lives_domain_quantiles = quantise(healthy_lives_rank),
    healthy_places_rank = inverse_rank(healthy_places),
    healthy_places_domain_quantiles = quantise(healthy_places_rank),
    healthy_people_rank = inverse_rank(healthy_people),
    healthy_people_domain_quantiles = quantise(healthy_people_rank),
    overall_health_index_rank = inverse_rank(overall_health_index),
    health_inequalities_composite_quantiles = quantise(overall_health_index_rank)
  ) |>
  select(
    starts_with("county_"), 
    ends_with("quantiles")
  )

# ---- Worst scoring places ----
sco_worst_hli <-
  sco_hi_shp |> 
  as_tibble() |> 
  select(lad_name, healthy_lives_domain_quantiles) |> 
  filter(healthy_lives_domain_quantiles == 10) |> 
  pull(lad_name)

sco_worst_hpl <-
  sco_hi_shp |> 
  as_tibble() |> 
  select(lad_name, healthy_places_domain_quantiles) |> 
  filter(healthy_places_domain_quantiles == 10) |> 
  pull(lad_name)

sco_worst_hpe <-
  sco_hi_shp |> 
  as_tibble() |> 
  select(lad_name, healthy_people_domain_quantiles) |> 
  filter(healthy_people_domain_quantiles == 10) |> 
  pull(lad_name)

sco_worst_overall <-
  sco_hi_shp |> 
  as_tibble() |> 
  select(lad_name, health_inequalities_composite_quantiles) |> 
  filter(health_inequalities_composite_quantiles == 10) |> 
  pull(lad_name)

eng_worst_hli <-
  eng_hi_shp |> 
  as_tibble() |> 
  select(county_ua_name, healthy_lives_domain_quantiles) |> 
  filter(healthy_lives_domain_quantiles == 10) |> 
  pull(county_ua_name)

eng_worst_hpl <-
  eng_hi_shp |> 
  as_tibble() |> 
  select(county_ua_name, healthy_places_domain_quantiles) |> 
  filter(healthy_places_domain_quantiles == 10) |> 
  pull(county_ua_name)

eng_worst_hpe <-
  eng_hi_shp |> 
  as_tibble() |> 
  select(county_ua_name, healthy_people_domain_quantiles) |> 
  filter(healthy_people_domain_quantiles == 10) |> 
  pull(county_ua_name)

eng_worst_overall <-
  eng_hi_shp |> 
  as_tibble() |> 
  select(county_ua_name, health_inequalities_composite_quantiles) |> 
  filter(health_inequalities_composite_quantiles == 10) |> 
  pull(county_ua_name)

# ---- Plot ----
p1 <-
  ggplot() +
  geom_sf(
    data = eng_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = health_inequalities_composite_quantiles)
  ) +
  geom_sf(
    data = sco_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = health_inequalities_composite_quantiles)
  ) +
  scale_fill_viridis(
    breaks = seq(1, 10, by = 1),
    labels = seq(1, 10, by = 1),
    na.value = "transparent",
    option = "magma",
    name = expression(paste("Vulnerability \nDecile")),
    alpha = 0.8, # make fill a bit brighter
    begin = 0.1, # this option seems to be new (compared to 2016):
    # with this we can truncate the
    # color scale, so that extreme colors (very dark and very bright) are not
    # used, which makes the map a bit more aesthetic
    end = 0.9,
    discrete = F, # discrete classes, thus guide_legend instead of _colorbar
    direction = -1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)) +
  theme_map() +
  ggtitle('Overall Score')

p2 <-
  ggplot() +
  geom_sf(
    data = eng_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = healthy_lives_domain_quantiles)
  ) +
  geom_sf(
    data = sco_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = healthy_lives_domain_quantiles)
  ) +
  scale_fill_viridis(
    breaks = seq(1, 10, by = 1),
    labels = seq(1, 10, by = 1),
    na.value = "transparent",
    option = "magma",
    name = expression(paste("Vulnerability \nDecile")),
    alpha = 0.8, # make fill a bit brighter
    begin = 0.1, # this option seems to be new (compared to 2016):
    # with this we can truncate the
    # color scale, so that extreme colors (very dark and very bright) are not
    # used, which makes the map a bit more aesthetic
    end = 0.9,
    discrete = F, # discrete classes, thus guide_legend instead of _colorbar
    direction = -1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)) +
  theme_map() +
  labs(title = "Healthy Lives", subtitle = "(Risk factors)")

p3 <-
  ggplot() +
  geom_sf(
    data = eng_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = healthy_places_domain_quantiles)
  ) +
  geom_sf(
    data = sco_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = healthy_places_domain_quantiles)
  ) +
  scale_fill_viridis(
    breaks = seq(1, 10, by = 1),
    labels = seq(1, 10, by = 1),
    na.value = "transparent",
    option = "magma",
    name = expression(paste("Vulnerability \nDecile")),
    alpha = 0.8, # make fill a bit brighter
    begin = 0.1, # this option seems to be new (compared to 2016):
    # with this we can truncate the
    # color scale, so that extreme colors (very dark and very bright) are not
    # used, which makes the map a bit more aesthetic
    end = 0.9,
    discrete = F, # discrete classes, thus guide_legend instead of _colorbar
    direction = -1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)) +
  theme_map() +
  labs(title = "Healthy Places", subtitle = "(Wider determinants)")

p4 <-
  ggplot() +
  geom_sf(
    data = eng_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = healthy_people_domain_quantiles)
  ) +
  geom_sf(
    data = sco_hi_shp,
    colour = "black",
    size = .1,
    mapping = aes(fill = healthy_people_domain_quantiles)
  ) +
  scale_fill_viridis(
    breaks = seq(1, 10, by = 1),
    labels = seq(1, 10, by = 1),
    na.value = "transparent",
    option = "magma",
    name = expression(paste("Vulnerability \nDecile")),
    alpha = 0.8, # make fill a bit brighter
    begin = 0.1, # this option seems to be new (compared to 2016):
    # with this we can truncate the
    # color scale, so that extreme colors (very dark and very bright) are not
    # used, which makes the map a bit more aesthetic
    end = 0.9,
    discrete = F, # discrete classes, thus guide_legend instead of _colorbar
    direction = -1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)) +
  theme_map() +
  labs(title = "Healthy People", subtitle = "(Health Outcomes)")

p1 + p2 + p3 + p4 +
  plot_layout(guides = 'collect')
