# Activating Repositories & Packages ----

setRepositories()
library(sf)
library(osmdata)

city <- "Faisalabad"
fsd_boundary <- opq(city) %>%
  add_osm_feature(key = "admin_level", value = "6") %>% # Adjust admin level for cities
  osmdata_sf()
print(fsd_boundary)
fsd_boundary$osm_multipolygons


fsd_multipolygon <- fsd_boundary$osm_multipolygons
print(fsd_multipolygon$geometry)


fsd_multipolygon <- fsd_multipolygon %>% 
  dplyr::select(geometry)

print(fsd_multipolygon$geometry)

summary(fsd_multipolygon$geometry)


writexl::write_xlsx(fsd_multipolygon, "fsd_multipolygon.xlsx")



# Extracting coorcinates from a multipolygonal geometry -----

sf_object <- st_sf(fsd_multipolygon)

multipolygonal_coord <- st_coordinates(sf_object)
print(multipolygonal_coord)


multipolygonal_coord <- as.data.frame(multipolygonal_coord)

multipolygonal_coord$X


# Importing Data for Farms and Industries -----
arcgis_fsd <- arcgis_fsd %>% 
  dplyr::select(farm_name)
writexl::write_xlsx(arcgis_fsd, "farm_names.xlsx")
writexl::write_xlsx(locations, "locations.xlsx")

library(readxl)
final_farm_data <- read_excel("final_farm_data.xlsx")
View(final_farm_data)

final_farm_data[-c(38,39,40),] %>% 
  janitor::clean_names()


final_farm_data  <- final_farm_data[-c(38,39,40),] %>% 
  janitor::clean_names()


View(final_farm_data)

# Importing Data for the Drains ----


faisalabad <- "Faisalabad, Pakistan"

# Query OpenStreetMap for waterways with the tag "drain"
drain_data <- opq(faisalabad) %>%
  add_osm_feature(key = "waterway", value = "drain") %>%
  osmdata_sf()

# Extract the geometry of drains as an sf object
drains <- drain_data$osm_lines
drains
leaflet() %>%
  addTiles() %>%
  addPolylines(data = drains, color = "blue", weight = 2, opacity = 0.8)

# Creating a Color Blind Free Palette -----


# Define a color-blind-friendly palette with three colors
palette <- colorFactor(
  palette = c("red", "blue", "orange"),  # Orange, Blue, and Green
  domain = final_farm_data$type
)


# Creating popup style using HTML & CSS ----


# Create styled popups using HTML
final_farm_data$popups <- paste0(
  "<div style='font-size:16px; font-family:Arial; color:#333; line-height:1.5;'>",
  final_farm_data$type,  
  "</div>"
)



drains <- drains %>% 
  mutate(waterway = str_replace(waterway, "drain","Sewage Drain"))

drains$popups <- paste0(
  "<div style='font-size:16px; font-family:Arial; color:#333; line-height:1.5;'>",
  drains$waterway,  
  "</div>"
)
