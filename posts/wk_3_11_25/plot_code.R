## Loading Libraries

### Basics
library(tidyverse)    # Data wrangling, visualization, and pipes
library(csucistats)   # Custom utilities (e.g., for CSU Channel Islands)

## Loading Data

pixar_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv')
public_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/public_response.csv')


## Combining Data

pixar <- left_join(pixar_films, public_response)
descriptive(pixar)
test <- cat_stats(pixar$film_rating, tbl_df = TRUE)
