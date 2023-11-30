body = dashboardBody(
  tabsetPanel(id = 'tabs',
    
    ### Year on Year tab
   { tabPanel(
      'Year On Year',
      value = 'YOY',
      
      box(
        tableOutput('yoydata'),
        title = 'Month On Month',
        status = 'primary',
        solidHeader = T,
        width = 12,
        height = 150
      ),
      
      box(
        plotlyOutput('yoy', height = '550px'),
        title = 'YoY',
        solidHeader = T,
        status = 'primary',
        collapsible = T,
        width = 12
      )
      
    )},
    
   ### Products tab
   {
    tabPanel(
      'Products',
      value = 'PRODUCTS',
      box(
        plotlyOutput('plo', height = '800px'),
        title = 'Top 10',
        status = 'primary',
        solidHeader = T,
        width = 12,
        collapsible = T
      )
    )
     },
   
   ### Distribution tab
   {
    tabPanel(
      'Distribution Of Sales By Product Group',
      value = 'DISTRIBUTION',
      box(
        plotlyOutput('pie', height = '800px'),
        title = 'Distribution Of Sales By Product Group',
        status = 'primary',
        solidHeader = T,
        width = 12,
        collapsible = T
      )
    )
     },
   
   ### Top Products tab
   {
    tabPanel(
      'Top Products',
      value = 'TOP_PRODUCTS',
      box(plotlyOutput('topProductGraph',height = '550px'), width = 12),
      box('Sales',tableOutput('topProductsTableTotal'),
          'Quantity',tableOutput('topProductsTableQuantity'),width = 12)
    )
   },
   
   ### Data tab
   {
    tabPanel('Data', dataTableOutput(outputId = 'pout'), value = 'DATA')
   },
   
   ### Insights Tab
   {
     tabPanel('Insights',value = 'INSIGHTS',
     ## Highest Selling Product
     verbatimTextOutput('HiSa'),
     
     ## Expected highest month
     verbatimTextOutput('ExMo'),
     
     ##Minimum stocks expected for the month
     verbatimTextOutput('ExSt'),
     dataTableOutput('ExStTable')
     
   )
   }
    
  ))
