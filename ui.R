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