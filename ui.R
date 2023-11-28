source('preprocessing.R')

loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: myuser  Password: mypass"),
                     br(),
                     tags$code("Username: myuser1  Password: mypass1")
                   ))
)

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)




























ui <- dashboardPage(
 
  
  skin = 'purple',
  
  
 dashboardHeader(title = 'V1'),
 
 
     
    {
      dashboardSidebar( conditionalPanel('input.tabs !=5',
     
      conditionalPanel('input.tabs != "1"',
      selectizeInput('yearnum','Which year',choices = c('All',`Years`= list(tot_test2$year))),
      ),
      
      
      
      conditionalPanel("input.tabs != '3'",
      h6('To change from \'All\', click on \'All\' and then click delete or backspace'),
      selectInput(
        'pgroupin',
        'Product Group',
        choices = c('All', `Groups` = list(product_groups)),
        multiple = T,
        selected = 'All',
        selectize = T
        
      ),
      selectInput('pin','Product',
                  choices = c('All',`Products`=list(products$Name)),
                  multiple = T,
                  selected = 'All',
                  selectize = T),
      
      
      checkboxInput(inputId = 'highquantity',
                    'See high Quantity Items Only?'),
     conditionalPanel('input.highquantity == true' ,
      numericInput(
        'minquant',
        '
                        Set the minimum quantity',
        value = 1100,
        max = 500000,
        min = 0
      )),
      checkboxInput('undperf', 'See Under Performing SKUs'),
     conditionalPanel("input.undperf == true",
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
      ))
      )
      
      
    ),
    conditionalPanel('input.tabs == 5',
    selectInput('seltp','Choose Top Product', choices = c(head(unique(tot_test2$Product,10)))))
      )
      },
{dashboardBody( useShinyjs(),
    tabsetPanel(id = 'tabs',
                tabPanel('Year On Year',value = '1',
                         
                         box(
                         tableOutput('yoydata'),title = 'Month On Month',status = 'primary',solidHeader = T,width = 12,height = 150),
                        
                        box(
                         plotlyOutput('yoy',height = '550px'),title = 'YoY',solidHeader = T,status = 'primary',collapsible = T,width = 12)
                         
                         ),

                tabPanel('Products',value = '2',
                 box(
                 plotlyOutput('plo',height = '800px'),title = 'Top 10',status = 'primary',solidHeader = T,width = 12,collapsible = T)
                ),
                
                tabPanel('Top Products',value = '5',
                         box(plotlyOutput('tpout'),width=12),
                         box('Sales Values',tableOutput('tpoutdat1'),'Quantity',tableOutput('tpoutdat2'),width = 12)),
                tabPanel('Distribution Of Sales By Product Group',value = '3',
                 box(
                   plotlyOutput('pie',height = '800px'),title = 'Distribution Of Sales By Product Group',status = 'primary',solidHeader = T,width = 12,collapsible = T)
                 )
                
                ,
                tabPanel('Data', dataTableOutput(outputId = 'pout'), value = '4')
                
                
              )
              
  )
      }
  #tags$head(
   #     tags$style(HTML("
#      .main-sidebar {
#        position: fixed;
#        height: 100%;
#      }
#      .content-wrapper {
#        margin-left: 250px; /* Adjust this value based on your sidebar width */
#      }
#    "))
#      )
 

)

