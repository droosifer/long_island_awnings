
library('shinydashboard')
library('shiny')
library('scales')
library('ggplot2')
library('magrittr')
library('dplyr')
library('DT')
library('leaflet')


data <- read.csv(file = "final_sales_cord.csv", stringsAsFactors = FALSE)

##data <- read.csv(file = "C:/Users/ringo/Desktop/docker_shiny_app/app/final_sales_cord.csv", stringsAsFactors = FALSE)


zip_list <- data %>% 
  select(Town) %>% 
  distinct() %>% 
  pull() %>% 
  as.character()
