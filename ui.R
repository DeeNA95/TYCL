source('preprocessing.R')


ui <- dashboardPage(
  skin = 'purple',
  
 dashboardHeader(title = 'V1'),
  
     
    {
      dashboardSidebar(
     
      conditionalPanel('input.tabs != "1"',
      selectInput('yearnum','Which year',choices = c('All',`Years`= list(tot_test2$year))),
      ),
      conditionalPanel('input.tabs == 2',selectInput("sort_by", "Sort by:", choices = c("Quantity", "Total"), selected = "Quantity")),
      
      
      
      conditionalPanel("input.tabs != '3'",
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
      
      
      checkboxInput(inputId = 'highquantity',
                    'See high Quantity Items Only?'),
     conditionalPanel('input.highquantity == true' ,
      numericInput(
        'minquant',
        '
                        Set the minimum quantity',
        value = 1100,
        max = 500000,
        min = 0
      )),
      checkboxInput('undperf', 'See Under Performing SKUs'),
     conditionalPanel("input.undperf == true",
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
      ))
      )
      
      
    )
      },
{dashboardBody(
    tabsetPanel(id = 'tabs',
                tabPanel('Year On Year',value = '1',
                         
                         box(
                         tableOutput('yoydata'),title = 'Month On Month',status = 'primary',solidHeader = T,width = 12,height = 150),
                        
                        box(
                         plotlyOutput('yoy',height = '550px'),title = 'YoY',solidHeader = T,status = 'primary',collapsible = T,width = 12)
                         
                         ),

                tabPanel('Products',height = '10000px',value = '2',
                 box(
                 plotlyOutput('plo',height = '800px'),title = 'Top 10',status = 'primary',solidHeader = T,width = 12,collapsible = T)
                ),
                tabPanel('Distribution Of Sales By Product Group',value = '3',
                 box(
                   plotlyOutput('pie',height = '800px'),title = 'Distribution Of Sales By Product Group',status = 'primary',solidHeader = T,width = 12,collapsible = T)
                 )
                
                ,
                tabPanel('Data', dataTableOutput(outputId = 'pout'), value = '4')
                
                
              )
              
  )
      },tags$head(
        tags$style(HTML("
      .main-sidebar {
        position: fixed;
        height: 100%;
      }
      .content-wrapper {
        margin-left: 250px; /* Adjust this value based on your sidebar width */
      }
    "))
      )
  

)

