## Loading Libraries
library(tidyverse)
library(csucistats)
library(camcorder)
library(cartogram)
library(sf)
library(janitor)
library(maps)


## Loading and Cleaning Data 
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-08/care_state.csv') 
care_state <- df |> group_by(state) |> 
  summarise(mean = mean(score, na.rm = TRUE)) |> 
  ungroup()

pop_df <- readr::read_csv("posts/wk_4_8_25/pop.csv") |> 
  clean_names() |> 
  pivot_longer(alabama:puerto_rico, names_to = "State", values_to = "Pop") 

pop_us <- pop_df |> 
  mutate(state_1 = str_replace(State, "_", " "),
         State = str_to_title(state_1), 
         state = state.abb[match(State, state.name)]) 

df2 <- left_join(care_state, pop_us, by = "state")

## Viewing Data
glimpse(df2)

usa <- st_as_sf(map("state", fill=TRUE, plot =FALSE)) |> 
  mutate(State = str_to_title(ID))
usa_df <- left_join(usa, df2, by = "State") |> 
  st_as_sf() |>
  st_transform(crs = 5070) 

states_carto <- cartogram_cont(usa_df, "Pop", itermax = 10)

gg_record(
  dir = file.path("posts", "wk_4_8_25"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Plot using ggplot
ggplot() +
  geom_sf(data = states_carto, aes(fill = mean)) +
  scale_fill_viridis_c(option = 'D') +
  labs(
    title = "Medicare and Medicaid Services Avereage Scoring",
    subtitle = "Adjusted by State Population Size"
  ) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave(
  filename = file.path("posts", "wk_4_8_25", paste0("final", ".png")),
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)


## Saving GIF
gg_playback(
  name = file.path("posts", "wk_4_8_25", paste0("final", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)
