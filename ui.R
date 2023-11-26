source('preprocessing.R')


ui <- dashboardPage(
  skin = 'purple',
  
 dashboardHeader(title = 'V1'),
  
     
    {
      dashboardSidebar(
     
      
      selectInput('yearnum','Which year',choices = c('All',`Years`= list(tot_test2$year))),
      
      
      #selectInput('themess',
                 # label = 'theme',
                  #choices = list('darkly','cerulean') ,
                  #selected = 'darkly'),
      
      
      h6('To change from \'All\', click on \'All\' and then click delete or backspace'),
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
      
      
    )
      },
    {dashboardBody(
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
                
                
              )
              
  )
      },tags$head(
        tags$style(HTML("
      .main-sidebar {
        position: fixed;
        height: 100%;
      }
      .content-wrapper {
        margin-left: 230px; /* Adjust this value based on your sidebar width */
      }
    "))
      )
  

)

