sidebar = dashboardSidebar(
  conditionalPanel(
    'input.tabs != "5"',
    conditionalPanel(
      'input.tabs != "1"',
      selectizeInput('yearnum', 'Which year', choices = NULL),
    )
    ,
    
    
    
    conditionalPanel(
      "input.tabs != '3'",
      h6(
        'To change from \'All\', click on \'All\' and then click delete or backspace'
      ),
      selectInput(
        'pgroupin',
        'Product Group',
        choices = NULL,
        multiple = T,
        selected = 'All',
        selectize = T
        
      ),
      selectInput(
        'pin',
        'Product',
        choices = c('All', `Products` = list(products$Name)),
        multiple = T,
        selected = 'All',
        selectize = T
      ),
      
      conditionalPanel(
        'input.tabs == "2" || input.tabs == "4" ',
        checkboxInput(inputId = 'highquantity',
                      'See high Quantity Items Only?'),
        conditionalPanel(
          'input.highquantity == true' ,
          numericInput(
            'minquant',
            '
                        Set the minimum quantity',
            value = 1100,
            max = 500000,
            min = 0
          )
        ),
        checkboxInput('undperf', 'See Under Performing SKUs'),
        conditionalPanel(
          "input.undperf == true",
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
      )
    )
  ),
  conditionalPanel(
    'input.tabs == 5',
    selectInput('topProduct', 'Product', choices = head(unique(tot_test2$Product), 8))
  )
  
)