## Loading Libraries
library(tidyverse)
library(csucistats)
library(palmerpenguins)
library(camcorder)
## Loading and Cleaning Data 

df <- penguins |> drop_na()

## Recording the Data
gg_record(
  dir = file.path("posts", "wk_4_15_25"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Plot using ggplot
ggplot(df, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  stat_smooth(se = F) +
  facet_wrap(~island) +
  scale_x_continuous(breaks =  round(seq(min(df$flipper_length_mm), max(df$flipper_length_mm), length.out = 4))) +
  labs(x = "Flipper Length (mm)",
       y = "Body Mass (g)",
       title = "Flipper Length versus Body Mass",
       subtitle = "by Species and Island") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


ggsave(
  filename = file.path("posts", "wk_4_15_25", paste0("final", ".png")),
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)


## Saving GIF
gg_playback(
  name = file.path("posts", "wk_4_15_25", paste0("final", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)
