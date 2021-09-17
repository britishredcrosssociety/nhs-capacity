pkgload::load_all(".")
nhsCapacity()

# ---- This code is to be added to the app once completed ----
library(ggplot2)
library(ggiraph)

# No license on code: note this same code has been reused in the tidytext
# package  (excluded here to reduce dependencies/size).
source("https://raw.githubusercontent.com/dgrtwo/drlib/master/R/reorder_within.R")

selected_tactical_cell <-
  uk_long |>
  filter(geo_code == "E54000007") |>
  distinct(tactical_cell) |>
  pull(tactical_cell)

# This is calculated to vertically center the labels in geom_text below
num_tactical_cell_areas <-
  uk_long |>
  filter(tactical_cell == selected_tactical_cell) |>
  filter(grepl("rank$", variable)) |>
  distinct(geo_name) |>
  pull(geo_name) |>
  length()

uk_long |>
  filter(tactical_cell == selected_tactical_cell) |>
  filter(grepl("rank$", variable)) |>
  mutate(variable = as.factor(variable)) |>
  mutate(geo_name = reorder_within(geo_name, score, variable)) |>
  ggplot(
    aes(
      x = geo_name,
      y = score,
      colour = if_else(geo_code == "E54000007", "Red", "Blue")
    )
  ) +
  facet_wrap(vars(variable), scales = "free_y") +
  geom_segment(
    aes(x = geo_name, xend = geo_name, y = 0, yend = score),
    show.legend = FALSE
  ) +
  geom_point(
    size = 5,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(Red = "#AD1220", Blue = "#475C74")
  ) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = max(score) * .8, ymax = Inf),
    alpha = .025,
    fill = "#9CAAAE",
    color = NA,
    show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = max(score) * .2),
    alpha = .025,
    fill = "#475C74",
    color = NA,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      x = num_tactical_cell_areas / 2,
      y = max(score) * .9,
      label = "Worse Performance",
      angle = 270
    ),
    hjust = "middle", vjust = "middle",
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      x = num_tactical_cell_areas / 2,
      y = max(score) * .1,
      label = "Best Performance",
      angle = 270
    ),
    hjust = "middle", vjust = "middle",
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = NULL, y = NULL)

# To Do:
# Can the expand argument of scale_x_reordered (e.g., expand(c(0, .5))) be 
# altered to create space between the x axis label and plot to place the current
# geom_text labels? They currently clash with the lollipop plots. Alternatively
# can space be added before after the lollipop plots on the y-axis using
# scale_y_continous(expand(c(0.5, 0.5)))?

# Highlight to the user that:
#   - The red lollipop indicates their selected area
#   - The plots only show areas within the same tactical cell (will a good
#     plot title do the trick here?)

# Add another table / tab with the underlying data, in addition to the ranks