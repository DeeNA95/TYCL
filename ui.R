source('preprocessing.R')
#source('loginpage.R')
source('body.R')
source('sidebar.R')
source('header.R')



ui <- dashboardPage(
  
  header, sidebar, 
  body,
  skin = "blue")