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