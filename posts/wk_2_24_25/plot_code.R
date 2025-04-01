## Loading Libraries
library(tidyverse)
library(csucistats)
library(ggvoronoi)

## Loading Data
article_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/article_dat.csv')
model_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/model_dat.csv')


## Viewing Data
glimpse(article_dat)
glimpse(model_dat)




