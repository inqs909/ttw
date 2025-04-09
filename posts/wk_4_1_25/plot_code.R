## Loading Libraries
library(tidyverse)
library(csucistats)
library(ggridges)
library(camcorder)
library(ggimage)



## Loading Data
pokemon_df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv'
)

## Viewing Data
glimpse(pokemon_df)

## Names of variables
names(pokemon_df)

## Categorical Data
cat_stats(pokemon_df$egg_group_1)
cat_stats(pokemon_df$type_1)

## Numerical Data
num_stats(pokemon_df$height)
ggplot(pokemon_df, aes(height)) + 
  geom_density()

## Select Pokémon

## Eevee
images <- pokemon_df |> 
  filter(pokemon %in% c("jigglypuff", "eevee")) |> 
  mutate(type_1 = str_to_title(type_1))
images$base_experience <- 600
images$type_1 <- c("Fairy", "Normal")
## Recording Progress

gg_record(
  dir = file.path("posts", "wk_4_1_25"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

## Plotting 

ggplot(
    pokemon_df, 
    aes(x = base_experience, y = str_to_title(type_1), fill = factor(stat(quantile)))
  ) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_x_continuous(limits = c(-50, 725), expand = c(0, 0)) +
  geom_image(
    data = images,
    aes(x = base_experience, y = str_to_title(type_1), image = url_image),
    size = 0.25, inherit.aes = FALSE
  ) + 
  labs(
    y = NULL,
    x = "Base Experience",
    fill = NULL,
    title = "Pokémon Density Ridgeline Plot",
    subtitle = "Distribution of Base Experience by Pokémon Type"
  ) +
  
  scale_fill_viridis_d(name = "Quartiles", option = "C") +
  theme_bw()


## Saving Plot
ggsave(
  filename = file.path("posts", "wk_4_1_25", paste0("final", ".png")),
  height = 4,
  width = 4,
  units = "in",
  dpi = 300
)


## Saving GIF
gg_playback(
  name = file.path("posts", "wk_4_1_25", paste0("final", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)

