
source('preprocessing.R')




server <- function(input, output, session) {
  
  
  
  tout = reactive({
    if( "All" %in% input$pgroupin){
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
  
  touf = reactive({
    pdata = toud()
    pdata = pdata %>% arrange(Total)
    if(T == input$undperf){
      filter(pdata,
             if('Quantity' == input$undperftype){
               pdata$Quantity < input$undperfnum
             }else{
               pdata$Total < input$undperfnum
             })
    } else{
      pdata
    }
  })
  
  toug = reactive({
     
    if(T == input$undperf){
      touf()
    }else{
      toud()
    }
  })
 
  output$plo = renderPlot(
    ggplot( head(toug(),10), 
            aes(x = reorder(Product,desc(Total)), y = Total,fill = ProductGroup, 
                label = paste('GHs',Total,'\nQuantity:',Quantity)
            )
    )
    
    + geom_col() 
    
    + geom_label()
    
    + xlab('Products')
    
    + scale_fill_manual('Product Group',
                        values = c('lightgreen','azure','pink','beige','#FF0'))
    
    
    
  )
  output$pout = renderDataTable(toug())
  
  
  
}