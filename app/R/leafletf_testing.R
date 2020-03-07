library(leaflet)
library(magrittr)
library(tidyr)
library(dplyr)


icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

test_data_works %>% 
  dplyr::filter(Town == 'Amityville') %>% 
  leaflet() %>% 
  addTiles() %>% 
  setView(-73.4350, 40.7891,  zoom = 9) %>% 
  addMarkers(~lng, ~lat, label = ~as.character(Order_Category))

test_data_works <- read.csv(file = "C:/Users/ringo/Desktop/docker_shiny_app/app/final_sales_cord.csv")

table(final_sales_lat_long$Order_Category)


Town == "Hicksville"