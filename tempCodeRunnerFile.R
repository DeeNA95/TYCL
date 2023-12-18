shinyApp(
  ui = fluidPage(
    plotlyOutput("mainChart"),
    verbatimTextOutput("doubleClickOutput")
  ),
  server = function(input, output) {
    output$mainChart <- renderPlotly({ main_chart })
    
    observeEvent(input$doubleClick, {
      if (input$doubleClick) {
        showModal(modalDialog(
          title = "Double-Click Event",
          "You double-clicked on the pie chart!"
        ))
      }
    })
  }
)