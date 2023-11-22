source('preprocessing.R')


ui <- fluidPage(
  h1('Test Using Oct 23 data'),
  sidebarLayout(
    
    sidebarPanel(width = 2,
                 
                # dateRangeInput('drange','Date',start = min(test$date),
                               # max = max(test$date) ),     
                 
                 
                 
                 
                 selectInput('pgroupin',
                                'Product Group',
                                choices = c('All',`Groups` = list(product_groups)), 
                                multiple = T,
                                selected = 'All',
                                selectize = T
                 ),
                 br(),
                 
                 checkboxInput(inputId = 'highquantity',
                               'See high quantity Items Only?'),
                 
                 numericInput('minquant','
                        Set the minimum quantity you want to see',
                              value = 100,
                              max = 500,
                              min = 0),
                checkboxInput('undperf', 'See Under Performing SKUs'),
                selectInput('undperftype','Use Quantity Or Total Sales',
                            choices = list('Quantity','Total')),
                numericInput('undperfnum','What is the threshold for underperforming?',value = 10, min = 0)
                
                 
    ),
    mainPanel(width = 10,
              tabsetPanel(
                
                tabPanel('Graphs', h3('Top 10'), plotOutput('plo')),
                
                tabPanel('Data',dataTableOutput(outputId = 'pout'))
                
                
              ))))