# Activating repositories & required Packages -----

setRepositories()
library(tidyverse)
library(janitor)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)
library(ggmap)
library(sf)
library(htmlwidgets)

install.packages("rvest", dependencies = TRUE)
library(rvest)
install.packages("tidygeocoder", dependencies = TRUE)
library(tidygeocoder)
install.packages("usethis", dependencies = TRUE)
library(usethis)
install.packages("leaflet", dependencies = TRUE)
library(leaflet)
library(httr)
library(jsonlite)


usethis::edit_r_environ()
usethis::edit_git_ignore()

# Importing Dataset containing coordinates-----

library(readxl)
map_fsd_df <- read_excel("Poultry Farms_Industries_Drains_Google coordinates.xlsx", 
                         range = "A2:C53")
View(map_fsd_df)

map_fsd_df <- map_fsd_df %>% 
  drop_na() 


map_fsd_df <-   map_fsd_df %>% 
  janitor::clean_names()  %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) 

# feature engeering 
# st 49 , st 61, Kahloon Poultry farm, Mehboob Ilahi Poultry Farm

new_data <- data.frame(
  farm_name = "Kahloon Poultry Farm",
  latitude = 31.551071773964914,    
  longitude =73.1018045526821,
  stringsAsFactors = FALSE
)

map_fsd_df <- rbind(map_fsd_df,new_data)



arcgis_fsd <- map_fsd_df %>% 
  geocode(address = farm_name,
          method = 'arcgis') 


map_fsd_df %>% 
  geocode(address = farm_name,
          method = 'google') 


arcgis_fsd %>% 
  leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(~long , ~lat, popup = ~farm_name)

reverse_arcgis_fsd <- map_fsd_df %>% 
  reverse_geocode(lat = latitude, long = longitude, method = 'arcgis')

reverse_arcgis_fsd %>% geocode(address = address, method = 'google')






locations <- c("VPWM+X66, Mamu Kanjan Rd, Muridwala, Samundri, Murid Wala, Faisalabad, Punjab",
               "Chattha Poultry Farm, Faisalabad", 
               "Chack No, Teshil Jaranwala, 72 GB Main Satyana Road, Faisalabad, Punjab, Pakistan",
               "8575+43F, Unnamed Road, Faisalabad, Punjab, Pakistan",
               "1, Faisalabad, 38000, Pakistan", 
               "Chak 113 JB, Village, Faisalabad, Punjab 38000, Pakistan",
               "Awagat, Faisalabad, Punjab 38000, Pakistan",
               "6V2P+JC, Chak 267 RB Jalandhar Araian, Faisalabad, Punjab, Pakistan",
               "Jaranwala, Faisalabad, Punjab, Pakistan",
               "Faisalabad, Punjab, Pakistan",
               "C6FC+Q5M, Faisalabad Bypass, Faisalabad, Punjab, Pakistan",
               "9392+8FF, Faisalabad, Punjab, Pakistan",
               "F3J3+R6F, Faisalabad, Punjab, Pakistan",
               "Dasuha 242RB Faisalabad, Dasuha, Faisalabad, 38000, Pakistan",
               "242, Basti Dasuha, Faisalabad, Punjab, Pakistan",
               "CWCR+CJP, Faisalabad, Punjab, Pakistan",
               "Main bazar 2chak, Faisalabad, Punjab, Pakistan",
               "Milat chowk 167-B, Faisalabad, Punjab 38000, Pakistan",
               "85C4+8WX, Rāu Khānuāna, Faisalabad, Punjab, Pakistan",
               "CXW6+3MX, Chak 61 JB Dehroad, Faisalabad, Punjab, Pakistan",
               "Roshan wala, Faisalabad, Punjab 38000, Pakistan",
               "chak#109 GB Bazajawala, Jaranwala Rd, Faisalabad, Punjab, Pakistan",
               "Unnamed Road, Faisalabad, Punjab, Pakistan",
               "Okara Rd, Chak 72Gb, Punjab, Pakistan",
               "Faisalabad, Punjab 45000, Pakistan",
               "Gulfishan Colony, Faisalabad, Punjab, Pakistan",
               "Yasir Town, Faisalabad, Punjab, Pakistan",
               "28km Sheikhupura - Faisalabad Rd, Faisalabad, Punjab 38000, Pakistan",
               "1088/2 Jail Rd, Civil Lines, Faisalabad, Punjab, Pakistan",
               "Jaranwala Rd, Dhudiwala Sharqi, Faisalabad, Punjab 38000, Pakistan",
               "F3RG+49C, Sargodha Rd, Faisalabad, Punjab, Pakistan",
               "Lahore - Sheikhupura - Faisalabad Rd, Khurianwala, Faisalabad, Punjab, Pakistan",
               "3.5-KM Chak Jhumra Rd, Khurianwala, Faisalabad, Punjab 38000, Pakistan",
               "F42P+42J Published Published, Malikpur, Faisalabad, Punjab, Pakistan",
               "new, Lahore Rd, Nishatabad, Faisalabad, Punjab, Pakistan",
               "9WXW+V65, Fazal Elahi Rd, Faisalabad, Punjab, Pakistan",
               "Dar-ul-Ehsan Colony, Samundri Road, Batala Colony, Faisalabad, Punjab 38000, Pakistan",
               "Faisalabad, Punjab, Pakistan",
               "Madhuana Drain, Faisalabad",
               "Jhang, Punjab, Pakistan",
               "5-km Sadrgodha-Siddhar Bypass Road, Faisalabad, 38000, Pakistan",
               "94-A Small Industrial Estate Street, Punjab Small Industries Corporation, Faisalabad, Punjab, Pakistan",
               "G22X+4GQ, Punjab Small Industries Corporation, Faisalabad, Punjab, Pakistan")





writexl::write_xlsx(coordinates_fsd, "coordinates_data.xlsx")

locations <- as.data.frame(locations)


coordinates_fsd <- locations %>% 
  geocode(address = locations, method = 'google')

# Creating the map using Leaflet -----

coordinates_fsd %>% 
  leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles(providers$Stadia.Outdoors) %>% 
  addLayersControl(
    baseGroups = c( "Toner", "OSM"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
addMarkers(~long , ~lat, popup = ~locations) 


leaflet() %>% 
  addTiles(group = "Base Map") %>% 
  addProviderTiles(providers$Stadia.Outdoors) %>% 
  addMarkers(data = coordinates_fsd,
             lat = ~lat, 
             lng = ~long ,
             group = "locations") %>% 
  addPolygons(lat = ~multipolygonal_coord$X,
              lng = ~multipolygonal_coord$Y,
              color = "blue")


# Final plot version 1 ----


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = multipolygonal_coord, 
              lat = ~Y,
              lng = ~X, color = "green",  opacity = 0.8, weight =2) %>% 
  addProviderTiles(providers$Stadia.Outdoors) %>% 
  addCircleMarkers(data = final_farm_data, 
                   lat = ~lat, 
                   lng = ~long, 
                   color = ~palette(type),
                   radius = 4, 
                   fillOpacity = 0.8,
                   popup = 
                   ) %>% 
  addPolylines(data = drains, color = "purple", weight =3, opacity = 0.7) %>%
  addLegend( data = final_farm_data,
    "bottomright",
    pal = palette,
    values = ~type,
    title = "Sampling Sites",
    opacity = 1
  )


# Final plot version 2 -----

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = multipolygonal_coord, 
              lat = ~Y,
              lng = ~X, color = "green",  opacity = 0.8,
              weight =2) %>% 
  addProviderTiles( provider =  providers$Stadia.Outdoors,
                   options = providerTileOptions(api_key= "24ad6145-811b-4acb-8d4a-dcc1bd66dac5" )) %>% 
  addCircleMarkers(data = final_farm_data, 
                   lat = ~lat, 
                   lng = ~long, 
                   color = ~palette(type),
                   radius = 4, 
                   fillOpacity = 0.8,
                   popup = ~popups
  ) %>% 
  addPolylines(data = drains, color = "purple",
               weight =3, opacity = 0.7, popup = ~popups) %>%
  addLegend("bottomright",
            colors = c("red","blue","orange","purple"),
            labels = c("Pharmaceutical Industry",
                       "Poultry Farm", "Textile Industry",
                       "Sewage Drain"),
             title = "Sampling Sites",
             opacity = 1
  )





map <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = multipolygonal_coord, 
              lat = ~Y,
              lng = ~X, color = "green",  opacity = 0.8,
              weight =2) %>% 
  addProviderTiles( provider =  providers$Stadia.Outdoors,
                    options = providerTileOptions(api_key= "24ad6145-811b-4acb-8d4a-dcc1bd66dac5" )) %>% 
  addCircleMarkers(data = final_farm_data, 
                   lat = ~lat, 
                   lng = ~long, 
                   color = ~palette(type),
                   radius = 4, 
                   fillOpacity = 0.8,
                   popup = ~popups
  ) %>% 
  addPolylines(data = drains, color = "purple",
               weight =3, opacity = 0.7, popup = ~popups) %>%
  addLegend("bottomright",
            colors = c("red","blue","orange","purple"),
            labels = c("Pharmaceutical Industry",
                       "Poultry Farm", "Textile Industry",
                       "Sewage Drain"),
            title = "Sampling Sites",
            opacity = 1
  )


# Saving map as an html widget -----


saveWidget(map, "map.html", selfcontained = TRUE)


