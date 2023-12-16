source('preprocessing.R')
source('sidebar.R')
source('header.R')
source('body.R')
source('packages.R')

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
      position: fixed; /* Change from sticky to fixed */
      width: 100%;
      background-color: #333; /* Set your preferred background color */
      color: white; /* Set your preferred text color */
      padding: 1px; /* Adjust padding as needed */
    }
     
      .content-wrapper {
        margin-left: 15%; 
         margin-top: 60px; /* Adjust margin-top to accommodate header height */
      }
    ")
  )))
