body = dashboardBody(

  tabsetPanel(
    type = 'pills',
    id = 'tabs',

    ### Last Month
    tabPanel(
      'Month Analysis',
      value = 'PM',
      textOutput('monthtit'),
      box(title = 'Month Figures',width = '100%',
      HTML("<span style='font-size: 22px; text-decoration: underline;'><strong>Sales</strong></span>"),
      tableOutput('monanaltableT'),
      HTML("<span style='font-size: 22px; text-decoration: underline;'><strong>Quantity</strong></span>"),
      tableOutput('monanaltableQ'),solidHeader = T,
      style = "font-family: 'Open Sans', sans-serif;",
      align = 'center', status = 'warning'),
       box(width = '100%',title ='Variance',
      tableOutput('varmonth'), status = 'warning',solidHeader = T, align = 'center'),
      fluidRow(
        column(width = 6,
      box(solidHeader = T,status = 'primary',title = 'Top Products',plotlyOutput('tpmonthgraph'),width = '100%')),
      column(width = 6,
      box(solidHeader = T,status = 'primary',title = 'Distribution of Sales by Product Group',plotlyOutput('monthpie'),width = '100%'))),
      style = "font-family: 'Open Sans', sans-serif;",
    ),

    ### Year on Year tab
    tabPanel(
      title = 'Year On Year',
      value = 'YOY',

      box(
        tableOutput('yoydata'),
        title = 'Sales In GHs',
        status = 'warning',
        solidHeader = T,
        width = '100%', 
        style = "font-family: 'Open Sans', sans-serif; ",
        align = 'center'),
        
        box(
        tableOutput('yoyvar'),
        title = 'Variance',
        status = 'warning',
        width = '100%', 
        style = "font-family: 'Open Sans', sans-serif; ",
        align = 'center',solidHeader = T),

      box(
        plotlyOutput('yoy', height = '50%'),
        title = 'YoY',
        solidHeader = T,
        status = 'primary',
        collapsible = T,
        width = '100%'
      ),
      fluidRow(
        column(width = 6,
      box(
        plotlyOutput('yoytot'),
        title = 'Year on Year Totals',
        solidHeader = T,
        status = 'primary',
        collapsible = T,
        width = '50%'
      )), column(width = 6,
      box(
        plotlyOutput('pie'),
        title = 'Distribution Of Sales By Product Group',
        status = 'primary',
        solidHeader = T,
        width = '50%',
        collapsible = T
      ))
      )
    ),

    ### Products tab
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
    ),

    ### Top Products tab
    tabPanel(
      'Top Products',
      value = 'TOP_PRODUCTS',

      box(title = textOutput('tptitle'),
      HTML("<span style='font-size: 22px; text-decoration: underline;'><strong>Sales</strong></span>"),
      tableOutput('topProductsTableTotal'),
      status = 'warning',
      HTML("<span style='font-size: 22px; text-decoration: underline;'><strong>Quantity</strong></span>"),
      tableOutput('topProductsTableQuantity'),
      width = '100%',solidHeader = T,
      style = "font-family: 'Open Sans', sans-serif;",
      align = 'center' )
      ,
      box(title = textOutput('tptitlegraph'),plotlyOutput('topProductGraph',height = '450px'), width = '100%',solidHeader = T ,status = 'primary' )
    ),

    ### Cross-section tab
    tabPanel(
      'Cross-Section',
      value = 'CROSS',
      plotlyOutput('cross1')
    ),

    ### Insights Tab
    tabPanel(
      'Insights',
      value = 'INSIGHTS',

      ## AVg sales per working day
      box(
        tableOutput('AvgSaPD'),
        title = 'Daily Sales Average (Ghs)',
        width = '100%',
        collapsible = T,
        solidHeader = T,
        status = 'warning',
        style = "font-family: 'Open Sans', sans-serif;"
      ),

     box(width = '100%',title = 'Predicted Sales For 2024', status = 'danger', solidHeader = T,
      tableOutput('p24')
     ),

      ## Highest Selling Product
      box(
        textOutput('HiSa',container = tags$h4,inline = T),

        # Expected highest month
        textOutput('ExMo',container = tags$h4,inline = T),
        width = '100%',
        collapsible = T,
        solidHeader = T,
        status = 'info',
        style = "font-family: 'Open Sans', sans-serif;"
      ),

      ##Minimum stocks expected for the month
      box(
        title = textOutput('ExSt'),
        dataTableOutput('ExStTable'),width = '100%',
        collapsible = T,
        solidHeader = T,
        status = 'primary',style = "font-family: 'Open Sans', sans-serif;"
      )
    ),

    ### Data tab
    tabPanel(
      'Data',
      DTOutput(outputId = 'pout'),
      value = 'DATA',
      style = "background-color: white;"
    ),

    tabPa
  )
)
