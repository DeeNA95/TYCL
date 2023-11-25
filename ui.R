source('preprocessing.R')


ui <- fluidPage(
  #theme of the app
 
 
  
  h1('V1'),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      # dateRangeInput('drange','Date',start = min(test$date),
      # max = max(test$date) ),
      
      
      #selectInput('themess',
                 # label = 'theme',
                  #choices = list('darkly','cerulean') ,
                  #selected = 'darkly'),
      
      
      
      selectInput(
        'pgroupin',
        'Product Group',
        choices = c('All', `Groups` = list(product_groups)),
        multiple = T,
        selected = 'All',
        selectize = T
        
      ),
      selectInput('pin','Product',
                  choices = c('All',`Products`=list(products$Name)),
                  multiple = T,
                  selected = 'All',
                  selectize = T),
      br(),
      
      checkboxInput(inputId = 'highquantity',
                    'See high quantity Items Only?'),
      
      numericInput(
        'minquant',
        '
                        Set the minimum quantity you want to see',
        value = 1100,
        max = 500000,
        min = 0
      ),
      checkboxInput('undperf', 'See Under Performing SKUs'),
      selectInput(
        'undperftype',
        'Use Quantity Or Total Sales',
        choices = list('Quantity', 'Total')
      ),
      numericInput(
        'undperfnum',
        'What is the threshold for underperforming?',
        value = 10,
        min = 0
      )
      
      
    ),
    mainPanel(width = 10,
              tabsetPanel(
                tabPanel('Graphs',
                         h3('Top 10'),
                         plotOutput('plo'),
                         br(),
                         h3('Month On Month'),
                         tableOutput('yoydata'),
                        
                         h3('YoY'),
                         plotOutput('yoy')
                         
                         ),
                
                tabPanel('Data', dataTableOutput(outputId = 'pout'))
                
                
              ),
              textOutput('themesss'))
  ),
  #theme = shinytheme(theme = paste0("\'",textOutput('themesss'),"\'")

)