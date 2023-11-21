library(shiny)
library(shiny)

ui <- fluidPage(
  h1('hello world'),
  paste('work')
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)