source('preprocessing.R')
source('sidebar.R')
source('header.R')
source('body.R')


ui <- dashboardPage(
  skin = 'purple',
  header,
  sidebar,
  body,
  tags$head(tags$style(
    HTML("
      .main-sidebar {
        position: fixed;
        height: 100%;
      }
      
      .main-header {
      position: sticky;
      width: 100%;
      }
     
      .content-wrapper {
        margin-left: 15%; 
      }
    ")
  )))
