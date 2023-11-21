library(shiny)
library(shiny)

ui <- fluidPage(
  h1('hello world')
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)