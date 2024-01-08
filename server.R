library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyjs)
library(stringr)

products <- read_csv("data/product.csv")
sales <- read_csv("data/sales.csv")
# replacing ketchup etc to condiments
sales <- sales %>%
  mutate(Product_Group = ifelse(grepl("ketchup", Product_Group, ignore.case = TRUE), "Condiments", Product_Group))



server <- function(input, output, session) {

  #Reactive for data
  data = reactive({
    if ("All" %in% input$yearnum) {
      sales %>%
        summarise(
          Total = round(sum(Total), 2),
          Quantity = round(sum(Quantity), 2),
          .by = c(Name, Product_Group)
        ) %>%
        arrange(desc(Total))
    } else {
      sales %>%
        filter(Year %in% input$yearnum) %>%
        summarise(
          Total = round(sum(Total), 2),
          Quantity = round(sum(Quantity), 2),
          .by = c(Name, Product_Group)
        ) %>%
        arrange(desc(Total))
    }
  })

output$data <- renderTable(data())

  #Reactive for daily Data
  daily = reactive({
    if(F == input$yesterday){
      sales %>%
      filter(Date >= input$daterange[1] & Date <= input$daterange[2])%>%
      arrange(desc(Total))
    } else {
       sales %>%
    filter(Date == max(sales$Date)) %>%
    arrange(desc(Total))
    }

  })

  output$dailyoutput = renderDataTable(daily())

  dailysummary = reactive({
    daily() %>%
      mutate('Product Group' = Product_Group ) %>%
      summarise(Total = sum(Total), .by = 'Product Group') %>%
      arrange(desc(Total)) %>%
      add_row('Product Group' = "Total", Total = sum(.$Total)) %>%
      mutate(Total = comma_format()(Total)) %>%
      pivot_wider(
        names_from = 'Product Group',
        values_from = 'Total'
        )
  })

  output$dailysummary = renderTable(dailysummary())

  
}