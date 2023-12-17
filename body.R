body = dashboardBody(

  tabsetPanel(
    id = 'tabs',

    ### Year on Year tab
    { tabPanel(
      title = 'Year On Year',
      value = 'YOY',

      box(
        h4('Sales In GHs'),
        tableOutput('yoydata'),
        title = 'Month On Month',
        status = 'primary',
        solidHeader = T,
        width = '100%',
        #height = 'auto',
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
    tabPanel('Data', DTOutput(outputId = 'pout'), value = 'DATA')
   },
   
### Insights Tab
{ tabPanel('Insights',value = 'INSIGHTS',

      
      ## AVg sales per working day
      box(
        tableOutput('AvgSaPD'),
        title = 'Daily Sales Average (Ghs)',
        width = '100%',
        collapsible = T,
        solidHeader = T,
        status = 'primary'),

     ## Highest Selling Product
     box(textOutput('HiSa',container = tags$h4,inline = T),

     ## Expected highest month
     textOutput('ExMo',container = tags$h4,inline = T),
     width = '100%',
        collapsible = T,
        solidHeader = T,
        status = 'primary'),

     ##Minimum stocks expected for the month
     box(title = textOutput('ExSt'),
     dataTableOutput('ExStTable'),width = '100%',
        collapsible = T,
        solidHeader = T,
        status = 'primary')
   )
} 
)
#, includeCSS('styles.css')
)
