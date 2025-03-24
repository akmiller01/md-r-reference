list.of.packages <- c(
  "data.table", "tidyverse", "tibble", "tidyr", "dplyr", "rstudioapi","ggplot2", "scales",
  "sf","arcpullr", "leaflet", "htmlwidgets", "tidygeocoder"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

getCurrentFileLocation =  function()
{
  this_file = commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file = rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())
setwd("..")

# There are three important ways of importing geospatial data into R, and two useful ways of visualizing it

# The first way of importing is if you have a SHP file. These generally come in a folder with multiple
# files (shp, xml, dbf, etc.). It's important to make sure you keep them together.
us = st_read("data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
md = subset(us, STATEFP==24)

# The sf package loads shapefiles as a spatial data frame.
# This means it stores all the data the same way, but always keeps reference to
# the underlying reference system and geometry
# You can view the shapefile's data just like any other data frame
View(md)
# And you can preview the shapes with a simple ggplot with the geom_sf function
ggplot(md) + geom_sf()

# The second way of importing data is through a CSV that contains longitude and latitude.
# You just read the CSV as normal, and then you tell R which columns contain the coordinates
banks = read.csv("data/md_chartered_banks_geocoded.csv")
banks = st_as_sf(banks, coords = c("longitude", "latitude"), crs = 4326)
ggplot() + geom_sf(data=md) + geom_sf(data=banks)

# What do you do if you have addresses in your CSV but no longitude and latitude? You geocode!
# The package `tidygeocoder` has an easy to use "geocode" function that can be used like so:
banks = read.csv("data/md_chartered_banks_geocoded.csv")
banks[,c("latitude", "longitude")] = NULL # Remove existing lat/long
banks = banks %>%
  geocode(full_addr, method = 'arcgis', lat = latitude , long = longitude)
banks = st_as_sf(banks, coords = c("longitude", "latitude"), crs = 4326)

# The tidygeocoder relies on third-party APIs that have their own licensing concerns, and
# it can be a little slow. If all of your addresses are within Maryland, you can make use of the
# geocoding functions I wrote to plug into the State's geocoding REST API
source("code/util_md_geocode.R") # Load functions from other R scripts like so
banks = read.csv("data/md_chartered_banks_geocoded.csv")
banks[,c("latitude", "longitude")] = NULL # Remove existing lat/long

# My utility works just like tidygeocoder
banks = banks %>%
  md_geocode_singleline(full_addr)

# Note that the Maryland geocoding service returns x/y coordinates in 3857 reference system
# In order to get lat/long out, we need to transform it
banks = st_as_sf(banks, coords = c("md_geocode_location.x", "md_geocode_location.y"), crs = 3857)
banks = st_transform(banks, crs=4326)
ggplot() + geom_sf(data=md) + geom_sf(data=banks)

# A third way to import spatial data into R is through API services like ArcGIS
# The `arcpullr` package gives us a convenient way to query and pull data from
# a whole host of endpoints
# For example, median-family income census tracts from HUD
# https://hudgis-hud.opendata.arcgis.com/datasets/HUD::acs-5yr-socioeconomic-estimate-data-by-tract/api
mfi_tract_api_url = "https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/ACS_5YR_ESTIMATES_SOCIOECONOMIC_TRACT/FeatureServer/16"
md_mfi_tracts = get_spatial_layer(
  mfi_tract_api_url,
  where = "STATE='24'",
  out_fields = c(
    "GEOID", "B19113EST1"
  )
)
setnames(md_mfi_tracts, "B19113EST1", "Tract_MFI")
ggplot(md_mfi_tracts) + geom_sf()

# Or just the tracts that are eligible to receive grants under the ENOUGH-act
# (30% or higher child poverty and at least one bank with 80% poverty concentration)
enough_url = "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/Enough_Act_Child_Poverty_Census_Tracts/FeatureServer/135/"
enough = get_spatial_layer(
  enough_url,
  where="CHILD_POV_PCT > 29.99999 AND SCHOOL_BND_INT_CPG_TOTAL > 0"
)
ggplot(enough) + geom_sf()

# If you want to speak about census tracts in human-intelligible names instead of numbers
# give this dataset of Maryland GEOID names I've created a try. It is sourced from
# Open Street Map, and names tracts based on the locations contained within their polygons
geoid_names = fread("data/md_geoid_names.csv")
enough = merge(enough, geoid_names, by.x="GEOID20", by.y="tract_geoid")

# Which ENOUGH-eligible tract has the highest measured child poverty?
highest_index = which.max(enough$CHILD_POV_PCT)
enough$CHILD_POV_PCT[highest_index] # 84.7% child poverty in
enough$GEOID20[highest_index] # Census tract "24510080200"
enough$tract_name[highest_index] # Aka The neighborhood of Berea
enough$county_name[highest_index] # In Baltimore City

# Please cite Open Street Map when using these names, and note they might not always
# align with how a community refers to themselves.

# That's all well and good, but what use is a black and white outline?
# Let's first make some choropleth maps using ggplot

ggplot() +
  geom_sf(data=md_mfi_tracts, aes(fill=Tract_MFI), color="transparent", linewidth=0.1) +
  scale_fill_binned(
    labels=dollar
  ) +
  labs(fill="Median family\nincome (2023)") +
  theme_void() +
  theme(
    panel.background = element_blank(),  # Remove default background
    panel.grid = element_blank(),        # Remove grid lines
    axis.title = element_blank(),        # Remove axis labels
    axis.text = element_blank(),         # Remove axis text
    axis.ticks = element_blank(),        # Remove axis ticks
    legend.position = c(0.15, 0.35)
  )

# Save it as an SVG in the output folder
ggsave("output/md_mfi.svg")


# The leaflet package is also great for creating some quick interactives
# although it does take some setup
blues = c(
  "#050D5E",
  "#313A7E",
  "#5D679E",
  "#8A95BE",
  "#B6C2DE",
  "#E3F0FF"
)

# I like to try and create my own bins around the quantiles for visual contrast
quantile(md_mfi_tracts$Tract_MFI, probs=seq(0, 1, 0.2), na.rm=T)
md_mfi_tracts$bin = "More than $250k"
md_mfi_tracts$bin[which(md_mfi_tracts$Tract_MFI < 250000)] = "$150k - $250k"
md_mfi_tracts$bin[which(md_mfi_tracts$Tract_MFI < 150000)] = "$100k - $150k"
md_mfi_tracts$bin[which(md_mfi_tracts$Tract_MFI < 80000)] = "$80k - $100k"
md_mfi_tracts$bin[which(md_mfi_tracts$Tract_MFI < 60000)] = "$60k - $80k"
md_mfi_tracts$bin[which(md_mfi_tracts$Tract_MFI < 40000)] = "Less than $60k"

md_mfi_tracts$bin = factor(md_mfi_tracts$bin, levels=c(
  "More than $250k",
  "$150k - $250k",
  "$100k - $150k",
  "$80k - $100k",
  "$60k - $80k",
  "Less than $60k"
))

enough$label = "ENOUGH eligible"
enough.pal <- colorFactor("orange", domain=enough$label, levels="ENOUGH eligible")

pal <- colorFactor(
  blues,
  domain = md_mfi_tracts$bin,
  levels = c(
    "More than $250k",
    "$150k - $250k",
    "$100k - $150k",
    "$80k - $100k",
    "$60k - $80k",
    "Less than $60k"
  ))

m = leaflet(md_mfi_tracts) %>%
  setView(-76.625, 39.3, 8) %>%
  addProviderTiles(
    providers$Esri.WorldGrayCanvas,
  ) %>%
  addPolygons(
    fillColor = ~pal(bin),
    weight = 0.5,
    opacity = 1,
    color = "#000",
    dashArray = "",
    fillOpacity = 1,
    popup = ~dollar(Tract_MFI),
    popupOptions = popupOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto",
      closeOnClick = TRUE
    )
  ) %>%
  addPolylines(
    data=enough,
    color="orange"
  ) %>%
  addMarkers(
    data = banks, popup = ~Bank,
    group="State-chartered financial institutions"
  ) %>%
  addScaleBar() %>%
  addLayersControl(
    overlayGroups = c("State-chartered financial institutions"),
    options = layersControlOptions(collapsed = F)
  ) %>%
  addLegend(
    pal = pal, values = ~bin,
    opacity = 1,
    title = "Tract income",
    position = "bottomleft"
  ) %>%
  addLegend(
    data = enough,
    pal = enough.pal, values = ~label,
    opacity = 1,
    title = "ENOUGH tracts",
    position = "bottomleft"
  ) 
m

# And then you can use the saveWidget function to export an HTML website.
saveWidget(m, file="output/sample_map.html")

# Lastly, one important geospatial transformation is the `point-in-polygon` operator
# Where you need to find which polygon a point belongs to.
# Let's say we want to find which the MFI of the tract for each bank
# As long as our points and polygons are in the same spatial reference frame, we can calculate
# which polygon each point belongs to with st_intersects
md_mfi_tracts = st_make_valid(md_mfi_tracts) # Fix invalid geometry
bank_tract_intersects = st_intersects(banks, md_mfi_tracts)
bank_tract_indices = sapply(bank_tract_intersects, `[[`, 1)
banks$Tract_MFI = md_mfi_tracts$Tract_MFI[bank_tract_indices]

# There is much more you can do with spatial data beyond just intersections,
# including measuring spherical-geometric distance and spatial regressions.
# Feel free to reach out if you want to know more
