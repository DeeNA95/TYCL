ui <- navbarPage(
    title = tags$nav(
    style = "background-color: #d8bfd8;
            font-family: 'Pacifico', cursive;
            font-size: 40px;
            border-bottom: 2px solid #333;
            font-weight: bold;
            position: fixed; /* Change from sticky to fixed */
            width: 100%;
            color: #333333; /* Set your preferred text color */
            padding: 1px; /* Adjust padding as needed */
            margin-left: 13.65%;
            display: flex",  # Set the background color
    class = "main-header",  # Add a custom class for styling
    "TYCL Coldstore & Grocery"
  ),
  tag$sidebar('control'),
  
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
    )

)

server <- function(input, output, session) {

}

shinyApp(ui,server)