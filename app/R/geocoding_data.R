## Loading packages for geo coding addresses

library(leaflet)
library(data.table)
library(stringr)
library(httr)
library(dplyr)



##reading in data using

data <- data.table::fread(file = "total_sales.csv")

data$ID <- seq.int(nrow(data))

##address is needed as a string separated by +

addresses <- stringr::str_c(data$Address
                            , data$Town
                            , data$Zip
                            , sep = ','
                            )

names(addresses) <- as.character(seq.int(addresses))

addresses <- stringr::str_replace_all(addresses, " ", "+")

## generated API Key in GCP center. Saved as text file in PWD

api_key <- readr::read_file(file = "geocoding_api_key.txt")

## don't want to waste requests so I'll work off a subset 

subset_addresses <- addresses[1:10]

##build function to convert to long/lat

build_address <- function(address, key) {
  
  
  url <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  
  address <- address
  
  key <- key
  
  final_address <- paste0(url, address, '&key=', key)
  
  final_address
  
  
  
  
}

##build trst URL

url_to_test <- build_address(addresses[2], api_key) 


## get request for test URL

results <- GET(url_to_test)

## body of the results from the get request

results_body <- content(results)

## putting results in a desireable format

df <- results_body$results[[1]]$geometry$location %>% 
  tibble::enframe(name = 'type', value = 'cordinates') %>%
  tidyr::spread(key = 'type', value = 'cordinates')

## create empty DF to add results to

final_df <- data.frame()

## loop through adddresses, create URL, get data, add to data frame

for(i in 1:length(addresses)) {
  
  
  currrent_address <- build_address(addresses[i], api_key)
  
  
  results <- GET(currrent_address)
  
  
  results_body <- content(results)
  
  if(results_body$status == "ZERO_RESULTS" | results_body$status != "OK"){
    
    
    lat <- 0
    
    lng <- 0
    
    df <- cbind.data.frame(lat,lng)
    
    
  } else {
    
    
    df <- results_body$results[[1]]$geometry$location %>% 
      tibble::enframe(name = 'type', value = 'cordinates') %>%
      tidyr::spread(key = 'type', value = 'cordinates')
    

  }
  
 
  
  
  final_df[i,1] <- df[1,1]
  
  final_df[i,2] <- df[1,2]
  
  
}

## create final data frame 

final_sales_lat_long <- cbind.data.frame(data, final_df)

## change from list type to numeric

final_sales_lat_long$lat <- as.numeric(final_sales_lat_long$lat)

final_sales_lat_long$lng <- as.numeric(final_sales_lat_long$lng)

## write to csv

write.csv(final_sales_lat_long, file = "final_sales_cord.csv")

