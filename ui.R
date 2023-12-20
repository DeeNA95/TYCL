source('preprocessing.R')
source('sidebar.R')
source('header.R')
source('body.R')
source('packages.R')

ui <- dashboardPage(
  
 header,
  sidebar = sidebar,
  body,
 
  tags$head(
  tags$link(
    rel = "stylesheet",
    href = "styles.css",
    type = "text/css"
  ),
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Pacifico&display=swap",
    type = "text/css"
  ),
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600&display=swap",
    type = "text/css"
  )
)


    )
  
