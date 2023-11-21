library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(shinythemes)
products = read_csv('products.csv') 
product_groups = unique(products$ProductGroup)
products_headers = names(products)
products_headers[3] = "Code"
names(products) = products_headers
test = readxl::read_xlsx('test/Oct 23.xlsx')
test_clean1 = test %>% select(c(Code,Product,Quantity,Total))
test_clean2 = test_clean1 %>% filter(Total != "NA", Total != "Total")
test_clean3 = test_clean2 %>% left_join(products[,2:3], by = "Code")
oct23 = test_clean3 %>% filter(Code != 'NA')
oct23$Total = as.numeric(oct23$Total)
oct23$Quantity = as.numeric(oct23$Quantity)
oct23 = oct23 %>% arrange(desc(Total))















ui <- fluidPage(
  h1('Test Using Oct 23 data'),
  sidebarLayout(
    
    sidebarPanel(width = 2,
                 
                 dateRangeInput('drange','Date',start = min(test$date),
                                max = max(test$date) ),     
                 
                 
                 
                 
                 selectizeInput('pgroupin',
                                'Product Group',
                                choices = c('All',`Groups` = list(product_groups)), 
                                multiple = T,
                                selected = 'All'
                 ),
                 br(),
                 
                 checkboxInput(inputId = 'highquantity',
                               'See high quantity Items Only?'),
                 
                 numericInput('minquant','
                        Set the minimum quantity you want to see',
                              value = 100,
                              max = 500,
                              min = 0)
                 
    ),
    mainPanel(width = 10,
              tabsetPanel(
                
                tabPanel('graphs', h3('Top 10'), plotOutput('plo')),
                
                tabPanel('data',tableOutput(outputId = 'pout'))
                
                
              ))))

server <- function(input, output, session) {
  
  tout = reactive({
    if( 'All' %in% input$pgroupin){
      oct23
    } else{
      subset(oct23, ProductGroup %in% input$pgroupin)
    }
  })
  
  
  
  toud = reactive({
    cdata = tout()
    
    if( T == input$highquantity ){
      
      filter(cdata, cdata$Quantity > input$minquant)
    } else {
      cdata
    }
  })
  
  
  output$plo = renderPlot(
    ggplot( head(toud(),10), 
            aes(x = reorder(Product,desc(Total)), y = Total,fill = ProductGroup, 
                label = paste('GHs',Total,'\nQuantity:',Quantity)
            )
    )
    
    + geom_col() 
    
    + labs(title = 'Top 10 Products',tag = 'tag test',caption = 'cap test')
    
    + xlab('Products')
    
    + scale_fill_manual('Product Group',
                        values = c('lightgreen','azure','pink','beige','#FF0'))
    
    
    
  )
  output$pout = renderTable(toud())
  
  
  
}




shinyApp(ui, server)

