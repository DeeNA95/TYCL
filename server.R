source('preprocessing.R')

server <- function(input, output, session) {
  
  ### Reactives for Data and Products
  {
  ## Initialising for with year specified
  {
  Preac1 = reactive({
    tot_test2 %>% 
      group_by(Product,ProductGroup) %>% 
      subset(year == input$yearnum) %>% 
      summarise(Total = round(sum(Total),2),Quantity = round(sum(Quantity),2))%>% 
      arrange(desc(Total))
  })
  }
  
  ## Initialising for without year specified
  {
  Preac2 = reactive({
    tot_test2 %>% 
      group_by(Product,ProductGroup) %>% 
      summarise(Total = round(sum(Total),2),Quantity = round(sum(Quantity),2)) %>% 
      arrange(desc(Total))
  })
  }
  
  ## Year number functionality
  {
  Preac3 = reactive({
  if('All' %in% input$yearnum){
   Preac2()
  }else{
   Preac1()
  }
  })
  }
  
  ## Product Group functionality
  {
  Preac4 = reactive({
    if ("All" %in% input$pgroupin) {
      Preac3()
    } else{
      subset(Preac3(), ProductGroup %in% input$pgroupin)
    }
  })
  }
  
  ## High Quantity functionality
  {
  Preac5 = reactive({
    if (T == input$highquantity) {
      Preac4() %>% 
      filter(Quantity > input$minquant)
    } else {
      Preac4()
    }
  })
  }

  ## Product functionality
  {
  Preac6 = reactive({
    if('All' %in% input$pin){
    Preac5()
    } else{
      subset(Preac5(),Product %in% input$pin)
    }
  })
  }
  
  ## Underperforming functionality
  {
  Preac7 = reactive({
    if(T == input$undperf ){
    if('Quantity' == input$undperftype){
      subset(Preac6(),Quantity < input$undperfnum) %>% 
        arrange(Quantity)
    } else{
      filter(Preac6(),Total < input$undperfnum) %>% 
        arrange(Total)
    }
    } else {
      Preac6()
    }
  })
  }
  
  ## Product Type functionality
  {
  Preac8 = reactive({
    if("All" %in% input$ptype){
      Preac7()
    } else{
      subset(Preac7(), 
             Product %in% 
               tot_test2$Product[
                 grepl( paste(input$ptype,collapse='|'), 
                       tot_test2$Product,
                       ignore.case = T)])
    }
  })
  }
  }
  
  ### Graph For Products
  {
  output$plo = renderPlotly({
    plot_ly(
      head(Preac8(),10),
      x = ~reorder(Product,desc(if (T == input$highquantity) Quantity else if (T == input$undperf && 'Quantity' == input$undperftype) Quantity else Total)),
      y = ~(if (T == input$highquantity) Quantity else if (T == input$undperf && 'Quantity' == input$undperftype) Quantity else Total),
      type = "bar",
      color = ~ProductGroup,
      text = ~paste('GHs', round(Total/1000, 1), 'k', '\nQuantity: ', round(Quantity, 1)),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = 'Products', tickangle = -15),
        yaxis = list(title = if (T == input$highquantity) 'Quantity' else if (T == input$undperf && 'Quantity' == input$undperftype) 'Quantity' else 'Total'),
        showlegend = TRUE,
        legend = list(title = 'Product Group'),
        barmode = "stack" ,
        title= paste0(
          if('All' %in% input$pgroupin){
            paste('All Groups\n')
          } else{
            paste0(input$pgroupin,' \n')
          },
          if('All' %in% input$yearnum){
            paste('All Years')
          }else{
            paste0('20',input$yearnum)
          })
      )
  })
  }
    
  ### Data tab table
  {
  output$pout = renderDataTable(
    datatable(Preac8(),
              options = list(pageLength = 20),
              rownames = F),
    server = T )
  }
  
  ### Reactives for Year on Year Table
  {
  ## Year on Year initialisation
  {
    yoy1dat = reactive({
      left_join(tot_test2 %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(Total)) %>% 
        mutate(Total = comma_format()(as.numeric(Total)))%>% 
        pivot_wider(names_from = month,
                    values_from = c("Total"),
                    names_sep = "_") ,
        tot_test2 %>% 
          group_by(year) %>% 
          summarise(Total = sum(Total)) %>% 
          mutate(Total = comma_format()(as.numeric(Total))),by = 'year')
    })
  }
  
  ## Product Group functionality
  {
    yoy2dat = reactive({
      if('All' %in% input$pgroupin){
        yoy1dat()
      } else{
        left_join(tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(Total)) %>% 
          mutate(Total = comma_format()(as.numeric(Total)))%>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_")  ,
          tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
            group_by(year) %>% 
            summarise(Total = sum(Total)) %>% 
            mutate(Total = comma_format()(as.numeric(Total))),by = 'year')
          
      }
    })
  }
  
  ## Product Functionality
  {
    yoy3dat = reactive({
      if('All' %in% input$pin){
        yoy2dat()
      } else {
        left_join(tot_test2 %>% subset(Product %in% input$pin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(Total)) %>% 
          mutate(Total = comma_format()(as.numeric(Total)))%>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_") ,
          tot_test2 %>% subset(Product %in% input$pin) %>% 
            arrange(desc(year))%>% group_by(year) %>% 
            summarise(Total = sum(Total)) %>% 
            mutate(Total = comma_format()(as.numeric(Total))) ,by = 'year'
      )}
    })
  }
  }
   
  ### Table for Year On Year
  {
  output$yoydata = renderTable(yoy3dat(),striped = T,hover = T,digits = 1,na = '-',colnames = T,server = T)
  }
  
  ### Reactives for Year On Year Graph
  {
  ## Initialise and Product Group functionality
  {
  yoy1 = reactive({
    if(("All" %in% input$pgroupin ) ){
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
  }
  
  ## Product functionality
  {
  yoy2 = reactive({
    if( ("All" %in% input$pin)){
      yoy1()
    } else {
      tot_test2 %>% 
        subset(Product %in% input$pin) %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(Total))
    }
  })
  }
  }
  
  ### Graph for Year on Year
  {
  output$yoy = renderPlotly({
    plot_ly(data = yoy2(), x = ~month, y = ~Total, type = "bar", color = ~year,
            text = ~paste0('GHs ', round(Total/1000,1),'k\n','20',year),
            hoverinfo = text)%>%
      layout(xaxis = list(title = 'Month'), yaxis = list(title = 'Total'))
  })
  }
  
  ### Reactives for Distribution tab
  {
  DistReac = reactive({
    Preac7() %>% 
      group_by(ProductGroup) %>% 
      summarise(Total = sum(Total))
  })
  }
  
  ### Graph for Distribution tab
  {
  output$pie = renderPlotly({
    plot_ly(DistReac(),
            type = 'pie',
            labels = ~ProductGroup,
            values = ~Total
            ) %>%
      layout(title = paste0( if('All' %in% input$yearnum) paste('All years') else paste0('20',input$yearnum)))
  })
  }
  
  ### Reactives for Top Products tab
  {
  ## Initialise and filter functionality for Graph
  {
  TPreacGraph = reactive({
    tot_test2 %>% filter(Product %in% input$topProduct) %>% 
      group_by(month,year) %>% 
      summarise(Total = sum(Total))
  })
  }
  
  ## Initialise and filter for total table
  {
  TPreactabletotal = reactive({
    left_join(tot_test2 %>% filter(Product %in% input$topProduct) %>% 
      group_by(month,year) %>% 
      summarise(Total = sum(Total)) %>% 
        mutate(Total = comma_format()(as.numeric(Total))) %>% 
      pivot_wider(names_from = month,values_from = Total),
      tot_test2 %>% filter(Product %in% input$topProduct) %>% 
        group_by(year) %>% 
        summarise(Total = sum(Total)) %>% 
        mutate(Total = comma_format()(as.numeric(Total))), by = 'year')
  })
  }
  
  ## Initialise and filter for quantity table
  {
  TPreactablequantity = reactive({
    left_join(tot_test2 %>% filter(Product %in% input$topProduct) %>% 
                group_by(month,year) %>% 
                summarise(Quantity = sum(Quantity)) %>% 
                mutate(Quantity = comma_format()(as.numeric(Quantity))) %>% 
                pivot_wider(names_from = month,values_from = Quantity),
              tot_test2 %>% filter(Product %in% input$topProduct) %>% 
                group_by(year) %>% 
              summarise(Quantity = sum(Quantity)) %>% 
                mutate(Quantity = comma_format()(as.numeric(Quantity))), by = 'year')
  })
  }
  
  ## Graph for Top Products
  {
  output$topProductGraph = renderPlotly({
    plot_ly(TPreacGraph(),
            x = ~month,
            y = ~Total,
            type = 'bar',
            color = ~year) %>% 
      layout(title = input$topProduct)
  })
  }
  
  ##Total and Quantity tables
  {
  output$topProductsTableTotal = renderTable(TPreactabletotal(),na = '-',width = 12)
  output$topProductsTableQuantity = renderTable(TPreactablequantity(),na = '-',width = 12,)
  }
  }
  
  ### Reactives for Insights
  {
    HiSa = reactive({
      if('All' %in% input$pgroupin){
      tot_test2 %>% group_by(Product) %>% 
        summarise(Total = sum(Total)) %>%arrange(desc(Total)) %>%  head(1) %>% select(1)
      } else {
        tot_test2  %>% group_by(Product) %>% subset(ProductGroup %in% input$pgroupin) %>% summarise(Total = sum(Total)) %>% 
           arrange(desc(Total)) %>%  head(1) %>% select(1)
      }
    })
    
    ExMo = reactive({
      if('All' %in% input$pgroupin){
        tot_test2 %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>% arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      } else {
        tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>% arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      }
    })
    
    ExSt = reactive({
      if('All' %in% input$pgroupin){
        tot_test2 %>% 
          filter(year == 23) %>%filter(month %in% 'Oct') %>% arrange(desc(Quantity)) %>% mutate(ExQuantity =round( Quantity * 1.25,0)) %>% select(2,8)
      } else {
        tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          filter(year == 23) %>%filter(month %in% 'Oct') %>% arrange(desc(Quantity)) %>% mutate(ExQuantity =round( Quantity * 1.25,0)) %>% select(2,8)
      }
    })
  }
  
  ### text for Insights
  {
    ## high sales
    output$HiSa = renderText(paste('Our highest selling product in',if('All' %in% input$pgroupin) 'All Product Groups' else input$pgroupin,'is',as.vector(HiSa()),'\n'))
    
    ## highest month
    output$ExMo = renderText(paste('Expected highest month is',as.vector(ExMo()$month),'\nWe expect to sell Ghs',comma_format()(as.vector(ExMo()$ExTotal))))
    
    ## required stocks to sell thatmuch
    output$ExSt = renderText(paste('Required Stocks to meet planned sales for',as.vector(ExMo()$month)))
    
    output$ExStTable = renderDataTable(ExSt())
    
  }
  
  ### Server side select/selectize
  {
  ##server side select for yearnum
    {
  updateSelectizeInput(session, 'yearnum', choices = c('All', `Years` = list(tot_test2$year)), server = TRUE)
    }
    
  ## server side select for product
    {
  updateSelectInput(session, 'pin', choices = c('All', `Products` = list(products$Name)),selected = 'All')
    }
  }
  
  ### Observes
  {
  ## Observe for changing product options when product group is selected
  {
  observe({
    product_choice = subset(products, ProductGroup %in% input$pgroupin)
    updateSelectInput(inputId = 'pin',choices = c('All',product_choice$Name),selected = 'All')
  })
  }
  }
  
}