#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library('shinydashboard')
library('shiny')
library('scales')
library('ggplot2')
library('magrittr')
library('dplyr')

title <- tags$a(href='http://www.liawning.com/',
                 tags$img(src='lia-logo.jpg', width = 230, height = 50), 
                 target="_blank")


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Overview", tabName = "dashboard", icon = icon("globe")),
        menuItem("Monthly Sales", tabName = "sales", icon = icon("chart-line")),
        menuItem("High Performers", tabName = "hp", icon = icon("fire")),
        menuItem("Map", tabName = "map", icon = icon("map-pin"))
    )
)

body <- dashboardBody(
    
    
     tags$head(
         tags$link(rel = "stylesheet", type = "text/css", href = "custom_work.css")
     ),
    
    
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Dollars Overview"),
                fluidRow(
    
                    # Dynamic ValueBoxes
                    valueBoxOutput("gross_profit_box", width = 3),
                    valueBoxOutput("total_sales_box", width = 3),
                    valueBoxOutput("total_cost_box", width = 3),
                    valueBoxOutput("total_transactions_box", width = 3)
                    
                ),
                fluidRow(
                    
                    
                    column(width = 12,align = 'center',
                           plotOutput("total_sales_plot")
                           
                    )
                    
                    
                )
                
        ),
        
        tabItem(tabName = "sales",
                h2("Yearly Sales Overview"),
                fluidRow(plotOutput("monthly_sales_plot")
                       ),
                column(width = 12,radioButtons("year_selection", "Select Year", inline = TRUE, choices = as.vector(2014:2019))
                    )
                
        ),
        
        tabItem(tabName = "hp",
                h2("Top Contributos to Gross Profit"),
              column(width = 6,
                     plotOutput('hp_bar1')),
                 
               
              column(width = 6,
                     plotOutput('hp_bar2'))
                
                )
    )
)

# Put them together into a dashboardPage
dashboardPage(
    dashboardHeader(title = title, titleWidth = 230),
    sidebar,
    body
)
