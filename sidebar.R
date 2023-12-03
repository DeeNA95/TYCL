sidebar = dashboardSidebar(
  
  ### Year Number
  {
  ### YEAR NUMBER WORKS IN ALL TABS BUT YOY AND TOP_PRODUCTS
  
    conditionalPanel("input.tabs != 'YOY' && input.tabs != 'TOP_PRODUCTS' && input.tabs != 'INSIGHTS'",
      
      selectizeInput('yearnum', 'Which year', choices =NULL)
    )
    },
    
  ### Product Group, Product, note on how to change from all
  { 
  ### THIS INFORMATION IS ONLY NECESSARY WHEN NOT IN  TOP_PRODUCTS OR DISTRIBUTION 
   
  conditionalPanel("input.tabs != 'TOP_PRODUCTS' && input.tabs != 'DISTRIBUTION'",
      h6(
        'To change from \'All\', click on \'All\' and then click delete or backspace'
      ),
      
      selectInput(
        'pgroupin',
        'Product Group',
        choices = c('All', `Product Group` = list(product_groups)),
        multiple = T,
        selected = 'All',
        selectize = T
        
      ),
    
      selectInput(
        'pin',
        'Product',
        choices = NULL,
        multiple = T,
        selected = 'All',
        selectize = T
      ),
      
      )
},
  
  ### Product Type, High Quantity & Underperforming 
  
      #### THESE ARE ONLY NECESSARY IN PRODUCTS AND DATA
      ## product_type in preprocessing
    
    conditionalPanel('input.tabs == "INSIGHTS" || input.tabs == "PRODUCTS" || input.tabs == "DATA"',
                     selectInput(
                       'ptype',
                       'Product Type',
                       choices = product_type,
                       multiple = T,
                       selected = "All",
                       selectize = T
                     )),
{
    conditionalPanel("input.tabs == 'PRODUCTS' || input.tabs == 'DATA'",
                     
        checkboxInput(inputId = 'highquantity',
                      'See high Quantity Items Only?'),
  
        conditionalPanel(
          'input.highquantity == true' ,
          numericInput(
            'minquant',
          'Set the minimum quantity',
            value = 1100,
            max = 500000,
            min = 0
          ))
        ,
  
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
},

  ### Top Product
  {
  ###Only to be seen if in TOP_PRODUCTS
  conditionalPanel(
    'input.tabs == "TOP_PRODUCTS"',
    selectInput('topProduct', 'Product', choices = head(unique(tot_test2 %>% group_by(Product) %>%  summarise(ProductValue = sum(Total)) %>% arrange(desc(ProductValue)) %>% select(1)), 10))
  )
}
)