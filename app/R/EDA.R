##Data analysis File
library(tidyverse)
library(openxlsx)
library(plotly)

wd <- list()


wd$data <- paste0(here::here(),'/Data')


wd$output <- paste0(here::here(), '/Output')

sales <- read_xlsx(path = paste0(wd$data, "/total_sales.xlsx"))

##Plotting line of sales by month

##done in ggplot

sales <- sales %>%
  mutate_at(vars(Sale.Price:Deposit), as.numeric)

sales$Month_Yr <- format(as.Date(sales$Date), "%Y-%m")


sales %>%
  ##filter(Year == 2018) %>%
  group_by(Month_Yr) %>%
  summarise(Sale.Price = sum(Sale.Price)) %>%
  plot_ly(x = ~Month_Yr, y = ~Sale.Price,type = 'scatter', mode = 'lines') %>%
  layout(title = list(text = "Sales", font = list(color = "fff")),
         xaxis = list(title = "Date", color = "fff"),
         yaxis = list(title = "Sales (In Dollars)", color = "fff"),
         plot_bgcolor = '#3d3d3d',
         paper_bgcolor = '#3d3d3d')


sales %>%
    mutate(transaction = 1) %>%
    filter(Zip != 11944) %>% ##this zip presented extreme outliers with minimal zip codes
    group_by(Zip) %>%
    summarise(total_profit = sum(Gross.Profit),
              total_transactions = sum(transaction)) %>%
    filter(total_transactions >= 5) %>%
    mutate(gross_per_transaction = total_profit/total_transactions) %>%
    arrange(desc(gross_per_transaction)) %>%
    top_n(5,gross_per_transaction)%>%
    mutate(zip = as.character(Zip)) %>%
    plot_ly(x = ~zip, y = ~gross_per_transaction, type = 'bar', color = ~zip, showlegend = FALSE) %>%
    layout(title = list(text = "Gross Profit per Transaction", font = list(color = "fff")),
           xaxis = list(title = "Zip Code", color = "fff"),
           yaxis = list(title = "Gross Dollars (per Trans.)", color = "fff"),
           plot_bgcolor = '#3d3d3d',
           paper_bgcolor = '#3d3d3d')
  
  
sales %>%
    mutate(transaction = 1) %>%
    group_by(Town) %>%
    summarise(total_profit = sum(Gross.Profit),
              total_transactions = sum(transaction)) %>%
    filter(total_transactions >= 10) %>%
    mutate(gross_per_transaction = total_profit/total_transactions) %>%
    arrange(desc(gross_per_transaction)) %>%
    top_n(5,gross_per_transaction) %>%
    plot_ly(x = ~Town, y = ~gross_per_transaction, type = 'bar', color = ~Town, showlegend = FALSE) %>%
    layout(title = list(text = "Gross Profit per Transaction", font = list(color = "fff")),
           xaxis = list(title = "Town", color = "fff"),
           yaxis = list(title = "Gross Dollars (per Trans.)", color = "fff"),
           plot_bgcolor = '#3d3d3d',
           paper_bgcolor = '#3d3d3d') 
sales %>%
  group_by(Year) %>%
  summarise(total_profit = sum(Gross.Profit, na.rm = TRUE)) %>%
  plot_ly(x = ~Year, y = ~total_profit, type = 'bar') %>%
  layout(title = list(text = "Total Sales by Year", font = list(color = "fff")),
         xaxis = list(title = "Year", color = "fff"),
         yaxis = list(title = "Total Gross Profit", color = "fff"),
         plot_bgcolor = '#3d3d3d',
         paper_bgcolor = '#3d3d3d')
