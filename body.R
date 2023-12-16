body = dashboardBody(
  tabsetPanel(id = 'tabs',
    
    ### Year on Year tab
   { tabPanel(
      'Year On Year',
      value = 'YOY',
      
      box(
        h4('Sales In GHs'),
        tableOutput('yoydata'),
        title = 'Month On Month',
        status = 'primary',
        solidHeader = T,
        width = '100%',
        height = '150%',
        h4('Variance'),
        tableOutput('yoyvar'),
      ),
      
      box(
        plotlyOutput('yoy', height = '100%'),
        title = 'YoY',
        solidHeader = T,
        status = 'primary',
        collapsible = T,
        width = '100%'
      ),
      
      box(
        plotlyOutput('yoytot', height = '100%'),
        title = 'Year on Year Totals',
        solidHeader = T,
        status = 'primary',
        collapsible = T,
        width = '100%'
      )
      
    )},
    
   ### Products tab
   {
    tabPanel(
      'Products',
      value = 'PRODUCTS',
      box(
        plotlyOutput('plo',height = '750px'),
        title = textOutput('pnumtitle'),
        status = 'primary',
        solidHeader = T,
        width = '100%',
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
        width = '100%',
        collapsible = T
      )
    )
     },
   
   ### Top Products tab
   {
    tabPanel(
      'Top Products',
      value = 'TOP_PRODUCTS',
      box(title = textOutput('tptitle'),'Sales',tableOutput('topProductsTableTotal'),status = 'primary',
          'Quantity',tableOutput('topProductsTableQuantity'),width = '100%',solidHeader = T)
      ,
      box(title = textOutput('tptitlegraph'),plotlyOutput('topProductGraph',height = '450px'), width = '100%',solidHeader = T ,status = 'primary' ))
      
    
   },
   
   ### Data tab
   {
    tabPanel('Data', dataTableOutput(outputId = 'pout'), value = 'DATA')
   },
   
### Insights Tab
{ tabPanel('Insights',value = 'INSIGHTS',

      h3('Daily Sales Average (Ghs)'),
      ## AVg sales per working day
      tableOutput('AvgSaPD'),

     ## Highest Selling Product
     textOutput('HiSa',container = tags$h3,inline = T),

     ## Expected highest month
     verbatimTextOutput('ExMo'),

     ##Minimum stocks expected for the month
     verbatimTextOutput('ExSt'),
     dataTableOutput('ExStTable')
   )
} 
))
