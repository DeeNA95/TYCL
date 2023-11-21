server <- function(input, output, session) {
  
  tout = reactive({
    if( 'All' %in% input$pgroupin){
      oct23
    } else{
      subset(oct23, ProductGroup %in% input$pgroupin)
    }
  })
  
  
  
  toud = reactive({
    cdata = tout()
    
    if( T == input$highquantity ){
      
      filter(cdata, cdata$Quantity > input$minquant)
    } else {
      cdata
    }
  })
  
  
  output$plo = renderPlot(
    ggplot( head(toud(),10), 
            aes(x = reorder(Product,desc(Total)), y = Total,fill = ProductGroup, 
                label = paste('GHs',Total,'\nQuantity:',Quantity)
            )
    )
    
    + geom_col() 
    
    + labs(title = 'Top 10 Products',tag = 'tag test',caption = 'cap test')
    
    + xlab('Products')
    
    + scale_fill_manual('Product Group',
                        values = c('lightgreen','azure','pink','beige','#FF0'))
    
    
    
  )
  output$pout = renderTable(toud())
  
  
  
}