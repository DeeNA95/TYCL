sidebar <- dashboardSidebar(
  collapsed = T,
  uiOutput("sidebarpanel"),
  conditionalPanel(
    'input.tabs == c("1","2","3","4","5")',
    selectizeInput('yearnum', 'Which year', choices = c('All', `Years` = list(tot_test2$year))),
    
    
    h6(
      'To change from \'All\', click on \'All\' and then click delete or backspace'
    ),
    
    selectInput(
      'pgroupin',
      'Product Group',
      choices = c('All', `Groups` = list(product_groups)),
      multiple = T,
      selected = 'All',
      selectize = T
      
    ),
    selectInput(
      'pin',
      'Product',
      choices = c('All', `Products` =
                    list(products$Name)),
      multiple = T,
      selected = 'All',
      selectize = T
    ),
    
    
    checkboxInput(inputId = 'highquantity',
                  'See high Quantity Items Only?'),
    
    numericInput(
      'minquant',
      '
                        Set the minimum quantity',
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
    ),
    
    selectInput('seltp', 'Choose Top Product', choices = c(head(
      unique(tot_test2$Product, 10)
    )))
  )
) 