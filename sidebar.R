sidebar = dashboardSidebar(

width = '13.65%',

tags$h1("Controls",
  style = "text-align: center; 
             font-weight: bold; margin-top: 20px; 
             text-decoration: underline;"
),

### Year Number

### YEAR NUMBER WORKS IN ALL TABS BUT YOY AND TOP_PRODUCTS
conditionalPanel(
  condition = "input.tabs != 'TOP_PRODUCTS' && 
    input.tabs != 'INSIGHTS' && input.tabs != 'CROSS' ",
  selectizeInput(
    'yearnum',
    'Which year',
    choices = NULL
  )
),

### Product Group, Product, note on how to change from all

### THIS INFORMATION IS ONLY NECESSARY WHEN NOT IN  TOP_PRODUCTS OR DISTRIBUTION
conditionalPanel('input.tabs == "PRODUCTS" ',
                   numericInput('pnum','View Top?',value = 10, max = 25)),
conditionalPanel(
  condition = "input.tabs != 'TOP_PRODUCTS' &&
  input.tabs != 'DISTRIBUTION'  &&
  input.tabs != 'CROSS' &&
  input.tabs != 'PM'" ,
  selectInput(
    'pgroupin',
    'Product Group',
    choices = c('All',
                `Product Group` = list(product_groups)),
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
    )

  ),

  ### Product Type, High Quantity & Underperforming

  #### THESE ARE ONLY NECESSARY IN PRODUCTS AND DATA

  conditionalPanel('input.tabs == "INSIGHTS" ||
                    input.tabs == "PRODUCTS" ||
                    input.tabs == "DATA"',
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
                       )),

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

  ###Only to be seen if in TOP_PRODUCTS
  conditionalPanel(
    'input.tabs == "TOP_PRODUCTS"',
    selectInput('topProduct', 'Product',
      choices = list(`Meat/Fish` = top_products_meat$Product,
                     `Oil`= top_products_oil$Product,
                     `Rice` = top_products_rice$Product,
                     `Sugar`= top_products_sugar$Product)
    )
  ),

  conditionalPanel('input.tabs == "CROSS"',selectizeInput(
    'yearnum2',
    'Which year',
    choices = NULL
  ),
  selectInput('cp', 'Cross-section by?', choices = c('Month','Product Group'),selected = 'Product Group')),

conditionalPanel('input.tabs == "PM"',
  selectInput('month', 'Month?', choices = month.abb, selected = 'Nov'),

numericInput('pnum2','View Top?',value = 5, max = 25))








 

)