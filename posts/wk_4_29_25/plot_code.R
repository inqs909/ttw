## Loading Libraries
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(camcorder)


## Loading and Cleaning Data 
user2025 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-29/user2025.csv')

pdf <- user2025 |> 
  separate_longer_delim(keywords, delim = ",") |>
  mutate(keywords = str_trim(keywords, side = "both"),
         keywords = str_to_upper(keywords))


pdf_tbl <- table(pdf$keywords)
tbl_names <- pdf_tbl[pdf_tbl > 4] |> names()

df <- pdf |> filter(keywords %in% tbl_names) |> 
  select(speakers, keywords) |> 
  rename(from = speakers, to = keywords)


## Recording the Data
gg_record(
  dir = file.path("posts", "wk_4_29_25"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


g <- graph_from_data_frame(df, directed = FALSE)
V(g)$type <- V(g)$name %in% unique(df$from)  # TRUE = person, FALSE = event

tg <- as_tbl_graph(g)
label_sizes <- ifelse(V(g)$type, 0, 4)
node_sizes <- ifelse(V(g)$type, 2, 5) 

# Plot using ggplot
ggraph(tg, layout = "fr") +
  geom_edge_link(alpha = 0.6) +
  geom_node_point(aes(color = type), size = node_sizes) +
  geom_node_text(aes(label = name), size = label_sizes, repel = TRUE) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = c("#B48B9C", "#30d5c8"), labels = c("Keyword", "Speaker")) +
  ggtitle("Network of Speakers by Keywords")

ggsave(
  filename = file.path("posts", "wk_4_29_25", paste0("final", ".png")),
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)


## Saving GIF
gg_playback(
  name = file.path("posts", "wk_4_29_25", paste0("final", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)
