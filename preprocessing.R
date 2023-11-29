#packages
{
  library(shiny)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(shinyjs)
  library(stringr)
  library(shinythemes)
  library(readxl)
  library(dplyr)
  library(purrr)
  library(base)
  library(tidyr)
  library(scales)
  library(shinydashboard)
  library(plotly)
  library(shiny)
  
  library(DT)
  library(shinyjs)
  
  library(stringr)
}

##product list
{
  products = read_csv('products.csv')
  products = distinct(products)
  product_groups = unique(products$ProductGroup)
  products_headers = names(products)
  products_headers[3] = "Code"
  names(products) = products_headers
}



##monthlys over the years
{
  #load and clean up function
  {
    load_month = function(month, year) {
      # month should be in quotes and year should be 2 digits
      selection = c('Code', 'Product', 'Quantity', 'Total')
      
      read_xlsx(paste0('test/', month, ' ', year, '.xlsx'), trim_ws = T) %>%
        select(selection) %>%
        subset(!(Code %in% c('NA', 'Code')),Total != 'NA')  %>%
       filter(Total != 'NA', Quantity != 'Quantity') %>%

        mutate(
          Code = as.character(Code),
          Total = as.numeric(Total),
          Quantity = as.numeric(Quantity),
          Product = as.character(Product)
        )
    }
  }
  
  #loading months
  
  {
    yars = c(22, 23)
    for (i in  month.abb) {
      for (y in yars) {
        if (file.exists(paste0('test/', i, ' ', y, '.xlsx')) == T) {
          processed_name <- paste0(i, y)
          
          assign(processed_name, load_month(i, y))
          
        }
      }
    }
  }
  
  
  
  
  
  
  tot_test = bind_rows(
    Jan22 %>% mutate(month = 'Jan', year = 22),
    Feb22 %>% mutate(month = 'Feb', year = 22),
    Mar22 %>% mutate(month = 'Mar', year = 22),
    Apr22 %>% mutate(month = 'Apr', year = 22),
    May22 %>% mutate(month = 'May', year = 22),
    Jun22 %>% mutate(month = 'Jun', year = 22),
    Jul22 %>% mutate(month = 'Jul', year = 22),
    Aug22 %>% mutate(month = 'Aug', year = 22),
    Sep22 %>% mutate(month = 'Sep', year = 22),
    Oct22 %>% mutate(month = 'Oct', year = 22),
    Nov22 %>% mutate(month = 'Nov', year = 22),
    Dec22 %>% mutate(month = 'Dec', year = 22),
    Jan23 %>% mutate(month = 'Jan', year = 23),
    Feb23 %>% mutate(month = 'Feb', year = 23),
    Mar23 %>% mutate(month = 'Mar', year = 23),
    Apr23 %>% mutate(month = 'Apr', year = 23),
    May23 %>% mutate(month = 'May', year = 23),
    Jun23 %>% mutate(month = 'Jun', year = 23),
    Jul23 %>% mutate(month = 'Jul', year = 23),
    Aug23 %>% mutate(month = 'Aug', year = 23),
    Sep23 %>% mutate(month = 'Sep', year = 23),
    Oct23 %>% mutate(month = 'Oct', year = 23),
    # un comment when Nov and dec exist
    #Nov23   %>% mutate(month = 'Nov',year = 23),
    #Dec %>% mutate(month = 'Dec',year = 23)
  )
  tot_test$month = factor(tot_test$month, levels = month.abb)
  tot_test$year = factor(tot_test$year, levels = c(22, 23))
  
  pgjoin = data.frame(products[, c(1:3)])
  
  
  tot_test2 = left_join(tot_test, pgjoin, by = "Code")
  tot_test2 = tot_test2 %>% select(-Name) %>% arrange(desc(Total))
  
  #beef tripe amalgamation
  
  tot_test2$Product = str_replace_all(
    tot_test2$Product,
    'Beef Tripe \\(Intestines\\)  Yemad3E',
    'Beef Tripe \\(Intestines\\) Yemad3E'
  )
  
  
  
  
 
}










