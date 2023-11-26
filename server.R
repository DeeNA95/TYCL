

source('preprocessing.R')




server <- function(input, output, session) {
  ###For current year display Graphs
  touy = reactive({
    tot_test2 %>% 
      group_by(Product,ProductGroup) %>% 
      subset(year == input$yearnum) %>% 
      summarise(Total = sum(Total),Quantity = sum(Quantity)) %>% 
      arrange(desc(Total))
  })
  touo = reactive({
    tot_test2 %>% 
      group_by(Product,ProductGroup) %>% 
      summarise(Total = sum(Total),Quantity = sum(Quantity)) %>% 
      arrange(desc(Total))
    
  })
  
  tout = reactive({
  if('All' %in% input$yearnum){
   touo()
  }else{
   touy()
  }
    
  })
  toug = reactive({
    if ("All" %in% input$pgroupin) {
      tout()
    } else{
      subset(tout(), ProductGroup %in% input$pgroupin)
    }
  })
  
  
  
  toud = reactive({
    
    if (T == input$highquantity) {
      toug() %>% 
      filter(Quantity > input$minquant)
    } else {
      toug()
    }
    
  })
  
  toup = reactive({
    if('All' %in% input$pin){
    toud()
    } else{
      subset(toud(),Product %in% input$pin)
    }
  })
  
  touf = reactive({
    if(T == input$undperf){
    if('Quantity' == input$undperftype){
      subset(toup(),Quantity < input$undperfnum) %>% 
        arrange(Quantity)
    } else{
      filter(toup(),Total < input$undperfnum) %>% 
        arrange(Total)
    }
    } else {
      toup()
    }
  })
  
  
  
  
  output$plo = renderPlot({
    
    ggplot( head(touf(),10) , aes(x = reorder(Product,desc(if(F == input$highquantity){
      
      Total
    } else {
      Quantity
    })), y =  if(T == input$highquantity){
      Quantity
    } else {
      Total
    },
      fill = ProductGroup,
      label = paste0('GHs', round(Total/1000,2),'k', '\nQuantity: ', round(Quantity,1))
      )) +
      geom_bar(stat = "identity")+
      geom_label()+
    
     xlab('Products')+
      ylab(if(T == input$highquantity){
        'Quantity'
      } else if(T == input$undperf){
        if('Quantity' == input$undperftype){
        'Quantity'
        }
      } else{
        'Total'
      })+
      
       scale_fill_manual(
        'Product Group',
        values = c(
          'lightgreen',
          'azure',
          'pink',
          'beige',
          'orange',
          'red',
          'white',
          'blue'
        )
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
    
  ##current year datatable
  output$pout = renderDataTable(touf())
  
  
  ###YoY analysis
  
  #reactives for yoy\
  {
    
    yoy1dat = reactive({
      tot_test2 %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(Total)) %>% 
        mutate(Total = comma_format()(as.numeric(Total)))%>% 
        pivot_wider(names_from = month,
                    values_from = c("Total"),
                    names_sep = "_") 
    })
    
    yoy2dat = reactive({
      if('All' %in% input$pgroupin){
        yoy1dat()
      } else{
        tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(Total)) %>% 
          mutate(Total = comma_format()(as.numeric(Total)))%>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_")  
          
      }
    })
    
    
    
    yoy3dat = reactive({
      if('All' == input$pin){
        yoy2dat()
      } else {
        tot_test2 %>% subset(Product[1] %in% input$pin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(Total)) %>% 
          mutate(Total = comma_format()(as.numeric(Total)))%>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_") 
      }
    })
    
  output$yoydata = renderTable(yoy3dat())
  
    
  yoy1 = reactive({
    if("All" %in% input$pgroupin){
    tot_test2 %>% 
      group_by(month,year) %>% 
        summarise(Total = sum(Total))
    } else {
      tot_test2 %>% 
        subset(ProductGroup %in% input$pgroupin) %>% 
      group_by(month,year) %>% 
        summarise(Total = sum(Total))
    }
  })
  
  
  
  #graph for yoy
  {
  output$yoy = renderPlot({
    
    ggplot(yoy1(), aes(x = month, y = Total, fill = year,
                       label = paste('GHs', round(Total/1000,1))))+
      geom_col( position = "dodge") +
      xlab('month') +
      ylab('total') +
      scale_fill_manual(
        'Year',
        values = c(
          'lightgreen',
          
          'pink',
          'beige',
          'orange',
          'red',
          'white',
          'blue'
        )
      )
      
  })
  
  }
  }
}