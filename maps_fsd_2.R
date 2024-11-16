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




