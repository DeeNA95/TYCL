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

products <- read_csv("data/product.csv")
sales <- read_csv("data/sales.csv")



server <- function(input, output, session) {

  ### Reactives for Data and Products
  product1 = reactive({
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

  product2 <- reactive({

  })


}