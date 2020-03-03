##previous year data ingestion


##Load libraires
library(readxl)
library(here)
library(tidyverse)
library(lubridate)


##Create working directory list - works with project to allow to be in any folder
wd <- list()
wd$data <- paste0(here::here(),'/Data')
wd$output <- paste0(here::here(), '/Output')


##load data sent by compoany 
prior_sales <- read_xlsx(paste0(wd$data, '/prior sales.xlsx'), col_names = FALSE)

##with dates

coltypes = c('date',rep('guess', 21))

prior_sales_dates <- read_xlsx(paste0(wd$data, '/prior sales.xlsx'), col_names = FALSE, col_types = coltypes)

dates <- prior_sales_dates[,1]

dates <- as.data.frame(as.character(dates$...1))

##Adding dates to original file

full_data <- cbind.data.frame(prior_sales, dates)


##Start cleaning data


##column names were very ugly, renaming them 
colnames(full_data) <- paste0("Col_",seq(1:ncol(full_data)))


#looking at the first column each year of data separated is separated by a new line
#in the first column where the new line contains 2010,2011,2012......


##this will be a good way to identify the row numbers to split the df at, or at least to distinguish the data
find_rows <- as.data.frame(str_locate(full_data$Col_1, pattern = "^20")) ##DF's are easier to work with 


##testing the row locationsm
row_splits <- which(find_rows == 1, arr.ind = TRUE)[,1]


##Splitting the new data frames to mark the year
listOfDfs <- list() 


for (i in 1:length(row_splits)) {
  
  
  new_data <- full_data %>%
    slice(row_splits[i]:row_splits[i + 1])
    
    
    listOfDfs[[i]] <- new_data
  

}


#Marking the year in each data set
NewDfList <- list()


for (i in 1:length(listOfDfs)){
  
  
  data <- listOfDfs[[i]]
  
  
  data$year <- as.character(data[1,1])
  
  
  NewDfList[[i]] <- data
    
    
}

##Cleaning individual files - might move to function based indivudal file creation later

##need to figure out how to bring in dates correctly

data_2010 <- NewDfList[[1]]

##removing column11 11 - 22

data_2010 <- data_2010 %>%
  select(-c(11:22))

##creating new column names
new_col_names <- as.character(data_2010[2,]) 


colnames(data_2010) <- new_col_names


data_2010 <- data_2010[-c(1,2,40,41),]


data_2010 <- data_2010 %>%
  rename('Year' =  `2010`,
         'Date' = `NA`)


##working on 2011 data cleaning

data_2011 <- NewDfList[[2]]

colnames(data_2011) <- new_col_names

data_2011 <- data_2011[-c(1, 109, 110), -c(11:22)]

colnames(data_2011)[11:12] <- c('Date','Year')

##working on 2012 data cleaning

data_2012 <- NewDfList[[3]]

colnames(data_2012) <- new_col_names

data_2012 <- data_2012[-c(1, 142), -c(11:22)]

colnames(data_2012)[11:12] <- c('Date','Year')


##cleaning 2013 data 

##column names start changing this year in comparison to the past 3 years

data_2013 <- NewDfList[[4]]

new_col_names_2013 <- as.character(data_2013[2,]) 

colnames(data_2013) <- new_col_names_2013

data_2013 <- data_2013[-c(1,2,243), -c(8:22)]

colnames(data_2013)[8:9] <- c('Date','Year')


##cleaning 2014 data

data_2014 <- NewDfList[[5]]

new_col_names_2014 <- as.character(data_2014[2,]) 

colnames(data_2014) <- new_col_names_2014

data_2014 <- data_2014[-c(1,2,208), -c(20:22)]

colnames(data_2014)[20:21] <- c('Date','Year')

##cleaning 2015 data

data_2015 <- NewDfList[[6]]

new_col_names_2015 <- as.character(data_2015[2,]) 

colnames(data_2015) <- new_col_names_2015

data_2015 <- data_2015[-c(1,2,317), -c(21:22)]

colnames(data_2015)[21:22] <- c('Date','Year')

##cleaning 2016 data

data_2016 <- NewDfList[[7]]

new_col_names_2016 <- as.character(data_2016[2,]) 

colnames(data_2016) <- new_col_names_2016

data_2016 <- data_2016[-c(1,2,443), -c(21:22)]

colnames(data_2016)[21:22] <- c('Date','Year')


##cleaning 2017 data

data_2017 <- NewDfList[[8]]

new_col_names_2017 <- as.character(data_2017[2,]) 

colnames(data_2017) <- new_col_names_2017

data_2017 <- data_2017[-c(1,2,482), -c(21:22)]

colnames(data_2017)[21:22] <- c('Date','Year')

##cleaning 2018 data

data_2018 <- NewDfList[[9]]

new_col_names_2018 <- as.character(data_2018[2,]) 

colnames(data_2018) <- new_col_names_2018

data_2018 <- data_2018[-c(1,2,473), -c(21:22)]

colnames(data_2018)[21:22] <- c('Date','Year')

