##read data into R
library(readxl)
library(here)
library(tidyverse)
library(lubridate)

wd <- list()


wd$data <- paste0(here::here(),'/Data')


wd$output <- paste0(here::here(), '/Output')


sales_2019 <- read_xlsx(paste0(wd$data, '/2019 Sales.xlsx'))


##Gotta seperate the order type column
str_view_all(sales_2019$Order, pattern = "(?<=\'|\")[:space:]")


##exploring the /' character for possible splits
str_view_all(sales_2019$Order, pattern = "\'")

str_starts(sales_2019$Order, pattern = "")

str_detect(sales_2019$Order, pattern = "[:digit:]")


removed_punc <- str_remove_all(sales_2019$Order, pattern = "x(?=[:space:])|&|-|\'|\"|[:digit:]|\\(|\\)[:punct:]")

removed_x <- str_remove_all(removed_punc, pattern = "x(?=[:space:])")

removed_x

table(removed_x)

final_form <- str_trim(removed_x, side = 'both' )

data <- as.data.frame(str_split_fixed(sales_2019$Order, pattern = "(?<=\'|\")[:space:]", 2))

##convert from weird format to date to then parse datetime using lubridate
sales_2019$Date <- as.character(sales_2019$Date)

sales_2019$Date <- as.Date(sales_2019$Date)

sales_2019$year_month <- format(sales_2019$Date, "%Y-%m")





