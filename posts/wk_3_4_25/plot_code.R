
## Loading Libraries

### Basics
library(tidyverse)    # Data wrangling, visualization, and pipes
library(csucistats)   # Custom utilities (e.g., for CSU Channel Islands)
library(scales)       # Tools for formatting scales (e.g., percent, commas)
library(utf8)         # Unicode support for emoji/text labels
library(paletteer)    # Access to many custom palettes (e.g., tayloRswift)
library(scatterpie)   # Adds the pie charts at different coordinates

### Geocomputation
library(sf)           # Simple Features for vector data (points, lines, polygons)
library(osmdata)      # Download OSM features via Overpass API
library(osmextract)   # Download OSM extracts (.pbf) and read spatial layers
library(ggmap)        # Geocoding and base map imagery (not used here)
library(terra)        # Raster/vector spatial analysis
library(tidyterra)    # Tidy syntax for terra operations

## Week Tag (for saving/exporting later)
week <- "wk_3_4_25"

## Loading Data
longbeach <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv'
)

## View structure of the shelter dataset
glimpse(longbeach)

## Define bounding box for Long Beach and download OSM data
bbox <- getbb("Long Beach, California", format_out = "sf_polygon") |> 
  st_bbox()

# Download SoCal OSM .pbf (cached locally)
pbf_path <- oe_get("southern california", download_only = TRUE)

### Extract "lines" (roads, rail, paths, etc.) from the .pbf file within bbox
long_beach_lines <- oe_read(
  file_path = pbf_path,
  layer = "lines",
  vectortranslate_options = c(
    "-spat", as.character(bbox["xmin"]), as.character(bbox["ymin"]),
             as.character(bbox["xmax"]), as.character(bbox["ymax"])
  )
) 

## Load Council District Shapefile
# Download this manually from the city's open data portal and unzip
council_districts <- st_read("~/Downloads/colb-council-districts/colb-council-districts.shp")

# Simplify and rename key attributes
longbeach_districts <- council_districts |>
  mutate(
    district = council_num,
    pop = population,
    area = shape_area,
    geometry = geometry,
    .keep = "none"
  )

## Query OSM for Parks within the bounding box
parks_query <- opq(bbox = bbox) |> 
  add_osm_feature(key = "leisure", value = "park")

# Download park polygons
parks <- osmdata_sf(parks_query)$osm_polygons

## Filter road types and match CRS across layers
long_beach_roads <- long_beach_lines |> filter(highway %in% c("primary", "secondary", "tertiary"))
longbeach_districts <- st_transform(longbeach_districts, st_crs(long_beach_roads))

# Intersect roads and parks with Long Beach council districts
long_beach_roads_districts <- st_intersection(long_beach_roads, longbeach_districts)
parks <- st_transform(parks, st_crs(long_beach_roads))
parks_lb <- st_intersection(parks, longbeach_districts)

## CSU Long Beach coordinates (manually input)
csulb_coords <- data.frame(
  name = "CSU Long Beach",
  lon = -118.11429280930847,
  lat = 33.78408779473669
)

# Convert to sf point
csulb_point <- st_as_sf(csulb_coords, coords = c("lon", "lat"), crs = 4326)
csulb_xy <- st_coordinates(csulb_point)

# Add label and coordinates
csulb_point$label <- utf8::utf8_encode("\U0001F393")
csulb_point$X <- csulb_xy[, "X"]
csulb_point$Y <- csulb_xy[, "Y"]

## Long Beach City College coordinates
lbcc_coords <- data.frame(
  name = "Long Beach City College",
  lon = -118.1730186086269,
  lat = 33.791834376134744,
  label = utf8::utf8_encode("\U0001F393")
)

# Convert to sf and match projection
lbcc_point <- st_as_sf(lbcc_coords, coords = c("lon", "lat"), crs = 4326)
lbcc_point <- st_transform(lbcc_point, st_crs(csulb_point))

# Add X and Y coordinates
lbcc_xy <- st_coordinates(lbcc_point)
lbcc_point$X <- lbcc_xy[, "X"]
lbcc_point$Y <- lbcc_xy[, "Y"]

# Combine both school locations
schools <- rbind(csulb_point, lbcc_point)

## Long Beach Pets

# Convert shelter data to sf points using lat/lon
lb_pets <- longbeach |> 
  select(animal_id, animal_type, latitude, longitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Reproject and join to districts
lb_pets <- st_transform(lb_pets, st_crs(longbeach_districts))
lb_pet_district <- st_join(lb_pets, longbeach_districts)

# Count animals by type and district
counts <- lb_pet_district |> 
  st_drop_geometry() |> 
  count(district, animal_type) |> 
  drop_na() |> 
  mutate(animal_type = case_when(
    animal_type == "amphibian" ~ "other",
    animal_type == "guinea pig" ~ "other",
    animal_type == "livestock" ~ "other",
    animal_type == "reptile" ~ "other",
    animal_type == "wild" ~ "other",
    animal_type == "other" ~ "other",
    .default = animal_type
  )) |> 
  group_by(district, animal_type) |> 
  summarise(sum = sum(n)) |> 
  ungroup() |> 
  pivot_wider(id_cols = district, names_from = animal_type, values_from = sum) |> 
  mutate(total = cat + dog + bird + rabbit + other)

## LB Centroid 
# Get spatial centroid for placing pie charts
lb_centroid <- st_centroid(longbeach_districts)

# Add coordinates to counts table
lb_counts <- left_join(counts, lb_centroid, by = "district") |> 
  mutate(
    longitude = st_coordinates(geometry)[, 1],
    latitude = st_coordinates(geometry)[, 2]
  ) |> 
  st_drop_geometry() |> 
  select(-geometry, -pop, -area)

## Plotting the Map
ggplot(data = long_beach_roads_districts) +
  
  # Plot the road segments in black
  geom_sf(color = "black", lwd = 0.1) +
  
  # Overlay the council district boundaries in dark red
  geom_sf(
    data = longbeach_districts,
    colour = "#871D20FF",  # dark red border
    fill = NA,             # no fill
    linewidth = 0.25       # thin border
  ) +
  
  # Add green park polygons with transparency
  geom_sf(
    data = parks_lb,
    fill = "forestgreen",
    color = NA,
    alpha = 0.35
  ) +
  
  # Add school location labels (e.g., CSU Long Beach, LBCC)
  geom_text(
    data = schools,
    aes(x = X, y = Y, label = label),
    size = 4
  ) +
  
  # Add pie charts showing pet distribution by district
  scatterpie::geom_scatterpie(
    data = lb_counts,
    aes(
      x = longitude,
      y = latitude,
      group = district
    ),
    cols = c("cat", "dog", "bird", "rabbit", "other"),  # pet types
    alpha = 0.9,
    donut_radius = 0.4,  # inner hole size (donut style)
    pie_scale = 4,       # scaling factor for pie size
    linewidth = 0.4      # border width of pie slices
  ) +
  
  # Apply a custom discrete fill palette with emoji-enhanced labels
  paletteer::scale_fill_paletteer_d(
    "tayloRswift::midnights",  # Taylor Swift "Midnights" palette
    labels = c(
      "cat" = paste("Cat", utf8::utf8_encode("\U0001F431")),
      "dog" = paste("Dog", utf8::utf8_encode("\U0001F436")),
      "bird" = paste("Bird", utf8::utf8_encode("\U0001F426")),
      "rabbit" = paste("Rabbit", utf8::utf8_encode("\U0001F430")),
      "other" = paste("Other", utf8::utf8_encode("\U0001F984"))
    )
  ) +
  
  # Add plot title and subtitle, remove axis labels
  labs(
    title = "Long Beach Animal Shelter",
    subtitle = "Pet Distribution by District",
    y = NULL,
    x = NULL
  ) +
  
  # Apply a clean black-and-white theme
  theme_bw() +
  
  # Move legend to bottom and remove title
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

## Saving Plot
ggsave(
  filename = paste0("final", ".png"),
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)

