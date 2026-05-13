library(dplyr)
library(ggplot2)
library(ggrepel)

round_trip_data <- read.csv("~/Desktop/MATH-397_Capstone/Cleaned_Up_data/round_trip_results.csv")

round_trip_plot_data <- round_trip_data %>%
  rename(
    origin_1 = Origin.1,
    dest_1 = Destination.1,
    distance_1 = Real.Distance..miles.,
    meters_1 = Real.Distance..meters.,
    origin_2 = Origin.2,
    dest_2 = Destination.2,
    distance_2 = Real.Distance..miles..1,
    meters_2 = Real.Distance..meters..1,
    imbalance_level = Imbalance_Level
  ) %>%
  filter(
    !is.na(origin_1),
    !is.na(dest_1),
    !is.na(distance_1),
    !is.na(distance_2)
  ) %>%
  mutate(
    trip_label = paste0(origin_1, " ↔ ", dest_1),
    distance_difference = abs(distance_1 - distance_2)
  )

ggplot(round_trip_plot_data, aes(
  x = distance_1,
  y = distance_2,
  color = imbalance_level,
  label = trip_label
)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.8
  ) +
  geom_point(size = 4, alpha = 0.85) +
  geom_text_repel(
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  coord_equal() +
  labs(
    title = "Directional Distance Balance for Common Inferred Round Trips",
    subtitle = "Points closer to the dashed 45-degree line indicate more balanced bidirectional travel distances",
    x = "Distance from Origin 1 to Destination 1 (miles)",
    y = "Distance from Origin 2 to Destination 2 (miles)",
    color = "Imbalance Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
