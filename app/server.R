

library('shinydashboard')
library('shiny')
library('scales')
library('ggplot2')
library('magrittr')
library('dplyr')

shinyServer(function(input, output) {
   
   
   sales <- data %>%
      mutate_each(as.numeric, 8:11) %>%
      mutate(Town = as.character(Town))
   
   
   sales$Month_Yr <- format(as.Date(sales$Date), "%Y-%m")
   
   
   gross_profit <- round(sum(sales$Gross.Profit, na.rm = TRUE))
   total_cost <- round(sum(sales$Total.Cost, na.rm = TRUE))
   total_transactions <- nrow(sales)
   total_sales <- round(sum(sales$Sale.Price, na.rm = TRUE))
   

   output$gross_profit_box <- renderValueBox({
      
      valueBox(
       paste0(dollar_format()(gross_profit)),
       "Gross Profit",
       icon = icon("money",class = 'fas fa-money fa-xs'),
       width = 6,
       color = 'green'
       
       
   )})
   
   
   output$total_cost_box <- renderValueBox({
      
      
       valueBox(
       paste0(dollar_format()(total_cost)),
       "Total Cost",
       icon = icon("money-bill-alt",class = 'fas fa-money-bill-alt fa-xs'),
       width = 6,
       color = 'red'
       
       
   )})
   
   
   output$total_transactions_box <- renderValueBox({
      
      
      valueBox(
      total_transactions,
      "Total Transactions",
      icon = icon("handshake",class = 'fas fa-handshake fa-xs'),
      width = 6,
      color = 'orange'
      
      
   )})
   
   output$total_sales_box <- renderValueBox({
      
      
      valueBox(
         paste0(dollar_format()(total_sales)),
         "Total Sales",
         icon = icon("handshake",class = 'fas fa-handshake fa-xs'),
         width = 6,
         color = 'orange'
         
         
      )})
   
   
   output$total_sales_plot <- renderPlot({
      
      
      sales %>%
         ##filter(Year == 2018) %>%
         group_by(Month_Yr) %>%
         summarise(Sale.Price = sum(Sale.Price)) %>%
         ggplot(aes(x = Month_Yr, y = Sale.Price, group = 1)) +
         geom_line() +
         theme_minimal()
      ##make tick levels on x axis at an angle
      
   })
   
   output$monthly_sales_plot <- renderPlot({
      
      
      sales %>%
         filter(Year == input$year_selection) %>%
         group_by(Month_Yr) %>%
         summarise(Sale.Price = sum(Sale.Price)) %>%
         ggplot(aes(x = Month_Yr, y = Sale.Price)) +
         geom_bar(stat = 'identity', fill = 'blue') +
         theme_minimal()
      
      
   })
   
   output$hp_bar1 <- renderPlot({
      
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
         ggplot(aes(x = zip, y = gross_per_transaction, fill = zip)) +
         geom_bar(stat = 'identity')
      
      
   })
   
   output$hp_bar2 <- renderPlot({
      
      sales %>%
         mutate(transaction = 1) %>%
         group_by(Town) %>%
         summarise(total_profit = sum(Gross.Profit),
                   total_transactions = sum(transaction)) %>%
         filter(total_transactions >= 10) %>%
         mutate(gross_per_transaction = total_profit/total_transactions) %>%
         arrange(desc(gross_per_transaction)) %>%
         top_n(5,gross_per_transaction) %>%
         ggplot(aes(x = Town, y = gross_per_transaction, fill = Town)) + 
         geom_bar(stat = 'identity')
      
      
   })
   

})
