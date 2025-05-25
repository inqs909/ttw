## Loading Libraries
library(tidyverse)
library(camcorder)
library(VoronoiPlus)

## Week Info

wk <- "wk_5_6_25"

## Loading and Cleaning Data 
nsf_terminations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')

## Recording the Data
gg_record(
  dir = file.path("posts", wk),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Plot using ggplot

ggsave(
  filename = file.path("posts", wk, paste0("final", ".png")),
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)


## Saving GIF
gg_playback(
  name = file.path("posts", wk, paste0("final", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)




