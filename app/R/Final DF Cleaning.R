library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx)

##combininf ytd sales with previous sales 

##reading in previous sales

wd <- list()


wd$data <- paste0(here::here(),'/Data')


wd$output <- paste0(here::here(), '/Output')


prior_data <- read.csv2(file = paste0(wd$data, "/cleaned_data.csv"), sep = ",")

##get rid of row names

prior_data <- prior_data[,-1]

##loading in YTD data

sales_2019 <- read_xlsx(paste0(wd$data, '/2019 Sales.xlsx'))

##getting rid of salesperson paid and email, not consistent data across previous years

sales_2019 <- sales_2019[,1:20]

sales_2019$Year <- 2019

colnames(sales_2019) <- colnames(prior_data)

new_sales <- sales_2019 %>%
  mutate_all(as.character)

sales <- rbind.data.frame(new_sales,prior_data)

##have to edit year for full date column where year is 2018, data didnt have year field so
##R auto set to sys date

in_2018 <- sales %>%
  filter(Year == 2018) %>%
  mutate(Date = as.Date(Date))

year(in_2018$Date) <- 2018

not_in_2018 <- sales %>%
  filter(Year != 2018) %>%
  mutate(Date = as.Date(Date))

sales <- rbind.data.frame(in_2018, not_in_2018)

sales <- sales %>%
  select(-c(Install.Date, Deposit.Date , Status, Balance, Notes, Order.Date))

##saving data set to not risk losing

write.csv(sales, file = paste0(wd$data, '/sales_before_order_clean.csv'))

##getting the order category/awning type
##getting the awning size

sales2 <- read.csv2(file = paste0(wd$data, '/sales_before_order_clean.csv'), sep = ',')

str_view_all(sales$Order, pattern = "\'")

removed_punc <- str_remove_all(sales$Order, pattern = "x(?=[:space:])|&|-|\'|\"|[:digit:]|\\(|\\)[:punct:]")

removed_x <- str_remove_all(removed_punc, pattern = "x(?=[:space:])")

removed_parenthesis <- str_remove_all(removed_x, pattern = "\\)")

final_form <- str_trim(removed_parenthesis, side = 'both' )

sales$Order_Category <- final_form

removed_letters <- str_remove_all(sales$Order, pattern = "[^x|[:digit:]|\'|\"]")

removed_letters_split <- str_split_fixed(removed_letters, pattern = "x|X", n = 3)

splits <- as.data.frame(removed_letters_split)

write.xlsx(sales,file = paste0(wd$data, "/total_sales.xlsx"))

