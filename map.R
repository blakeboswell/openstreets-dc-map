library(tidyverse)
library(sf)
library(osmdata)
library(tigris)
library(tidycensus)
library(showtext)

font_add_google("Petit Formal Script", "Petit Formal Script")
showtext_auto()

options(tigris_use_cache = TRUE)


# get major and minor streets as separate tables
# for ease of assigning different series aesthetics

streets <- getbb("Washington DC") %>%
  opq() %>%
  add_osm_feature(
    key   = "highway", 
    value = c(
      "motorway"
      , "primary"
      , "secondary"
      , "tertiary"
    )
  ) %>%
  osmdata_sf()

small_streets <- getbb("Washington DC") %>%
  opq() %>%
  add_osm_feature(
    key   = "highway", 
    value = c(
      "residential"
      , "living_street"
      , "service"
      , "footway"
    )
  ) %>%
  osmdata_sf()


# get the set difference of DC proper to add alpha transparency

dc_tract    <- tigris::tracts(state = "DC")
dc_boundary <- st_union(dc_tract)
dc_inv      <- st_difference(st_as_sfc(st_bbox(dc_boundary)), dc_boundary)


# cool places in DC to add as points

tables <- tribble(
  ~description,~x,~y,
  "C&O Canal Bike Trail"    , -77.100427, 38.919322,
  "Georgetown Waterfront"   , -77.065318, 38.902852,
  "Blues Alley Club"        , -77.062226, 38.904724 + 0.0025,
  "Kennedy Center"          , -77.054550, 38.896093, 
  "Phillips Collection"     , -77.046668, 38.911803,
  "National Cathedral"      , -77.071093, 38.930475,
  "Hains Point"             , -77.025357, 38.867941, 
  "National Gallery of Art" , -77.019895, 38.890783, 
  "Beach Drive"             , -77.044682, 38.951690,
  "3 Stars Brewing"         , -77.012222, 38.967616,
  "Logan Circle"            , -77.029636, 38.909642,
  "Blagden Alley"           , -77.025076, 38.906461 - 0.0025, 
  "Union Market"            , -76.998126, 38.908665,
  "Georgia Ave"             , -77.028314, 38.956784,
  "Ivy City"                , -76.984294, 38.913934,
)

tables <- tables %>% 
  mutate(
    n = str_pad(as.character(row_number()), width = 2, pad = "0")
  )

tables %>% write_csv('output/locations.csv')

tables <- st_as_sf(tables, coords = c("x", "y"), crs = st_crs(4326)) %>%
  st_transform(crs = st_crs(dc_inv))

# make map

make_map <- function(background_color, large_color, small_color, font_color,
                     include_labels = FALSE) {
  
  p <- ggplot() +
    geom_sf(
      data = streets$osm_lines,
      inherit.aes = FALSE,
      color = large_color,
      size  = 0.4,
      alpha = 0.8
    ) +
    geom_sf(
      data = small_streets$osm_lines,
      inherit.aes = FALSE,
      color = small_color,
      size  = 0.2,
      alpha = 0.6
    ) +
    geom_sf(
      data = dc_inv,
      alpha = 0.75,
      fill  = background_color,
      color = NA
    ) +
    geom_sf(
      data = dc_boundary,
      fill  = NA,
      color = large_color,
      size  = 0.4
    ) +
    geom_sf(
      data = tables,
      color = "#4A6670",
      size = 1.5
    ) +
    coord_sf(
      xlim = c(-77.01331 - 0.095, -77.01331 + 0.09),
      ylim = c(38.88561 - 0.065, 38.88561 + 0.095)
    ) +
    theme_void() +
    theme(
      plot.caption = element_text(
        color = font_color,
        size  = 18,
        face  = "bold",
        hjust = 1,
        family = "Petit Formal Script",
        margin = margin(t = "10")
      ),
      plot.margin = unit(c(0.6, 1.6, 1, 1.6), "cm"),
      plot.background = element_rect(fill = NA, color = NA)
    ) +
    labs(
      caption = "Washington D.C."
    )

  if(include_labels) {
    p <- p +
      geom_sf_label(
        data = tables,
        aes(label = n),
        nudge_y = 0.002,
        check_overlap = TRUE,
        family = "Petit Formal Script",
      )
  }
  
  p
      
}


p <- make_map(
  background_color   = '#EDF6F9',
  large_color        = '#006d77',
  small_color        = '#83c5be',
  font_color         = '#006d77',
  include_labels = TRUE
)

# save svg and pdf versions

ggsave(file = './output/map-large.svg', plot = p, width = 8.5, height = 11, bg = "white")

ggsave(
  plot = p,
  filename = 'output/map.pdf', 
  device="pdf",
  width = 8.5,
  height = 11,
  )



