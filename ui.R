source('preprocessing.R')
source('sidebar.R')
source('header.R')
source('body.R')
source('packages.R')

ui <- dashboardPage(
  
 header,
  sidebar = sidebar,
  body,
 
  tags$head(tags$style(
    HTML("
      .sidebar {
        position: fixed;
        height: 100%;
        width: 13.65%;
        background-color: #2F4F4F;
        color: white;
        margin-top: -50px;
        border: 2px solid #333;
        border-top: none;
      }
      
     
     
    .content-wrapper {
      
      margin-top: 50px;
      background-color: azure;
      color: black;
      }
  
  "),

  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Pacifico&display=swap",
    type = "text/css"
  ),tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600&display=swap",
    type = "text/css"
  )
)

    )
  )
