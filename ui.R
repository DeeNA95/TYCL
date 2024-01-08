
ui <- navbarPage(#inverse = T,
  title = "thembayinebolc",
  tabPanel(
    "Data",
    sidebarPanel(
      width = 2,
      selectizeInput(
        'yearnum',
        'yearnum',
        choices = list('All', 2024)
      )
    ),
    mainPanel(
      tableOutput('data')
    )
  ),
  tabPanel(
    'Daily Data',
    sidebarPanel(
      width = 2,
      checkboxInput('yesterday', 'View Previous Working Day',T),
      conditionalPanel(
        condition = "input.yesterday != T",
        dateRangeInput(
        separator = '-',
        'daterange',
        'Date',
        start = Sys.Date(),
        end = Sys.Date(),
        min = min(sales$Date),
        max = Sys.Date()
      )
    )),
    mainPanel( width = 12-2,
      
      tableOutput('dailysummary'),
      dataTableOutput('dailyoutput')
    )
  ),
  tabPanel(
    'l'
  ),





    tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
)

    )
    
