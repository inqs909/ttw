## Loading Libraries

### Basics
library(tidyverse)
library(csucistats)
library(scales)
library(camcorder)
library(httr)
library()


### Geocomputation
library(sf)                  
library(osmdata)   
library(osmextract)
library(ggmap)               
library(terra)               
library(tidyterra)           

## Week
week <- "wk_3_4_25"

## Loading Data
longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')
## Viewing Data
glimpse(longbeach)

## Downloading Files

### Points
# socal_points <- oe_get(
#   place = "southern california",
#   layer = "points",
#   download_directory = "~/dropbox/Research/GIS_Data/socal/points/"         
# )

bbox <- getbb("Long Beach, California", format_out = "sf_polygon") |> st_bbox()


### Lines
long_beach_lines <- oe_read(
  file_path = "~/dropbox/Research/GIS_Data/socal/geofabrik_socal-latest.osm.pbf",
  layer = "lines",
  vectortranslate_options = c(
    "-spat", as.character(bbox["xmin"]), as.character(bbox["ymin"]),
             as.character(bbox["xmax"]), as.character(bbox["ymax"])
  )
) 

# Read the shapefile into an sf object
council_districts <- st_read("~/Downloads/colb-council-districts/colb-council-districts.shp")


longbeach_districts <- council_districts |>
  mutate(
    district = council_num,
    pop = population,
    area = shape_area,
    geometry = geometry,
    .keep = "none"
  )


parks_query <- opq(bbox = bbox) |> 
    add_osm_feature(key = "leisure", value = "park")
  
  # Download and extract polygons
parks <- osmdata_sf(parks_query)$osm_polygons

long_beach_roads <- long_beach_lines |> filter(highway %in% c("primary", "secondary", "tertiary"))
longbeach_districts <- st_transform(longbeach_districts, st_crs(long_beach_roads))
long_beach_roads_districts <- st_intersection(long_beach_roads, longbeach_districts)
parks <- st_transform(parks, st_crs(long_beach_roads))
parks_lb <- st_intersection(parks, longbeach_districts)

# CSU Long Beach coordinates
csulb_coords <- data.frame(
  name = "CSU Long Beach",
  lon = -118.11429280930847,
  lat = 33.78408779473669
)

# Convert to an sf point object with WGS84 (EPSG:4326)
csulb_point <- st_as_sf(csulb_coords, coords = c("lon", "lat"), crs = 4326)
csulb_xy <- st_coordinates(csulb_point)
# Convert to sf point
csulb_point$label <- utf8::utf8_encode("\U0001F393")
csulb_point$X <- csulb_xy[, "X"]
csulb_point$Y <- csulb_xy[, "Y"]

lbcc_coords <- data.frame(
  name = "Long Beach City College",
  lon = -118.1730186086269,
  lat = 33.791834376134744,
  label = "🎓"
)

# Convert to sf object
lbcc_point <- st_as_sf(lbcc_coords, coords = c("lon", "lat"), crs = 4326)

# Match CRS to base map
lbcc_point <- st_transform(lbcc_point, st_crs(csulb_point))  # or long_beach_lines

# Extract coordinates for plotting
lbcc_xy <- st_coordinates(lbcc_point)
lbcc_point$X <- lbcc_xy[, "X"]
lbcc_point$Y <- lbcc_xy[, "Y"]

schools <- rbind(csulb_point, lbcc_point)


## Long Beach Pets

lb_pets <- longbeach |> 
  select(animal_id, animal_type, latitude, longitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

lb_pets <- st_transform(lb_pets, st_crs(longbeach_districts))
lb_pet_district <- st_join(lb_pets, longbeach_districts)
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

lb_centroid <- st_centroid(longbeach_districts)

lb_counts <- left_join(counts, lb_centroid, by = "district") |> 
  mutate(longitude = st_coordinates(geometry)[,1], latitude = st_coordinates(geometry)[,2]) |> 
  st_drop_geometry() |> 
  select(-geometry, -pop, -area)


ggplot(data = long_beach_roads_districts) +
  geom_sf(color = "black", lwd = 0.1) +
  geom_sf(
    data = longbeach_districts,
    colour = "#871D20FF",
    fill = NA,
    linewidth = .25
  ) +
  geom_sf(data = parks_lb, fill = "forestgreen", color = NA, alpha = 0.35) +
  geom_text(
    data = schools,
    aes(x = X, y = Y, label = label),
    size = 4
  ) +
  scatterpie::geom_scatterpie(
    data = lb_counts,
    aes(
      x = longitude, 
      y = latitude, 
      group = district
    ),
    cols = c(
      "cat", "dog", "bird", "rabbit", "other"
    ),
    alpha = 0.9,
    donut_radius = 0.4,
    pie_scale = 4,
    linewidth  = 0.4
  ) +
  paletteer::scale_fill_paletteer_d("tayloRswift::midnights",
    labels = c("cat" = paste("Cat", utf8::utf8_encode("\U0001F431")),
               "dog" = paste("Dog", utf8::utf8_encode("\U0001F436")),
               "bird" = paste("Bird", utf8::utf8_encode("\U0001F426")),
               "rabbit" = paste("Rabbit", utf8::utf8_encode("\U0001F430")),
               "other" = paste("Other", utf8::utf8_encode("\U0001F984")))) +
  labs(title = "Long Beach Animal Shelter", subtitle = "Pet Distribution by District", y = NULL, x = NULL) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


## Recording Progress

gg_record(
  dir = file.path("posts", week),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

## Obtaining Basemap

## Saving Plot
ggsave(
  filename = file.path("posts", week, paste0("final", ".png")),
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)


## Saving GIF
gg_playback(
  name = file.path("posts", week, paste0("final", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)
