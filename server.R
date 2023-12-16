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
      head(Preac8(),input$pnum),
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
    eomnov = tot_test2 %>%
      group_by(year, month) %>%
      summarise(Total = sum(round(Total, 0))) %>%
      mutate(MTD = cumsum(Total)) %>%
      pivot_wider(names_from = month,
                  values_from = c("Total", "MTD"),
                  names_sep = "_") %>% select(21)
    
    yoy1dat = reactive({
     left_join(tot_test2 %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(round(Total,0))) %>% 
       #mutate(Total = comma_format()(as.numeric(Total)))%>% 
        pivot_wider(names_from = month,
                    values_from = c("Total"),
                    names_sep = "_") %>% rowwise() %>% 
        mutate('Total' = rowSums(pick(where(is.numeric)),na.rm = T)),
       eomnov,by = 'year')
    })
  }
  
  ## Product Group functionality
  {
    
    
    yoy2dat = reactive({
      if('All' %in% input$pgroupin){
        yoy1dat()
      } else{
        eomnov = tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
        group_by(year, month) %>%
        summarise(Total = sum(round(Total, 0))) %>%
        mutate(MTD = cumsum(Total)) %>%
        pivot_wider(names_from = month,
                    values_from = c("Total", "MTD"),
                    names_sep = "_") %>% select(21)
        
        left_join(tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(round(Total,0))) %>% 
          #mutate(Total = comma_format()(as.numeric(Total)))%>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_") %>% 
          mutate('Total' = rowSums(pick(where(is.numeric)),na.rm = T) ), eomnov,by = 'year')
          
      }
    })
  }
    
  ## Product Functionality
  {
    yoy3dat = reactive({
      if('All' %in% input$pin){
        yoy2dat()
      } else {
        eomnov = tot_test2 %>%
          group_by(year, month) %>%
          summarise(Total = sum(round(Total, 0))) %>%
          mutate(MTD = cumsum(Total)) %>%
          pivot_wider(names_from = month,
                      values_from = c("Total", "MTD"),
                      names_sep = "_") %>% select(21)
        
        left_join(tot_test2 %>%  subset(Product %in% input$pin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(round(Total,0)) )%>% 
          #mutate(Total = comma_format()(as.numeric(Total)))%>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_") %>% 
          mutate('Total' = rowSums(pick(where(is.numeric)),na.rm = T) ),eomnov, by = 'year')
    }
      })
  
  }
  }
  
  ### Reactives for Year on Year Variance Table
  {
      ## initialise
      {
    yoyvardat = reactive({
      t1 = tot_test2 %>%
        group_by(month,year) %>%
        summarise(Total = sum(Total)) %>%  
        pivot_wider(names_from = year, values_from = "Total") %>% 
        mutate(percentage_change = ((`23` - `22`) / `22`) * 100) 
      
     outpre = t1 %>% 
        select(1,4)  %>% 
        mutate(
          Year = '23 vs 22',
          percentage_change = if_else(is.na(percentage_change),'-',paste0(round(percentage_change,1),'%')
          ))  %>%
        pivot_wider(names_from = month,values_from = percentage_change)
     
     novpc =tot_test2 %>%
       group_by(year, month) %>% 
       summarise(Total = sum(Total)) %>%
       mutate(MTD = cumsum(Total)) %>%
       pivot_wider(names_from = year, values_from = c("Total",'MTD') )%>%
       mutate('MTD %' = (MTD_23/MTD_22 -1) * 100) %>% select(1,4,5,6)
     
     MTD = paste0(round(novpc[8,4],1),'%')
     
      cbind(outpre,MTD)
     
    })
      }
      
      ## product group functionality
      {
    yoyvardat2 =reactive({
      if('All' %in% input$pgroupin){
        yoyvardat()
      } else {
        t3 =  tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          group_by(month,year) %>%
          summarise(Total = sum(Total)) %>%  
          pivot_wider(names_from = year, values_from = "Total") %>% 
          mutate(percentage_change = ((`23` - `22`) / `22`) * 100) 
        
        
       
        
       outpre2 = t3 %>% 
          select(1,4)  %>% 
          mutate(
            Year = '23 vs 22',
            percentage_change = if_else(is.na(percentage_change),'-',paste0(round(percentage_change,1),'%')
            ))  %>%
          pivot_wider(names_from = month,values_from = percentage_change)
       
       novpc =tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
         group_by(year, month) %>% 
         summarise(Total = sum(Total)) %>%
         mutate(MTD = cumsum(Total)) %>%
         pivot_wider(names_from = year, values_from = c("Total",'MTD') )%>%
         mutate('MTD %' = (MTD_23/MTD_22 -1) * 100) %>% select(1,4,5,6)
       
       MTD = paste0(round(novpc[8,4],1),'%')
       
       cbind(outpre2,MTD)
      }
    })
      }
      
      ## product functionality
      {
        yoyvardat3 = reactive({
          if('All' %in% input$pin){
            yoyvardat2()
          } else{
            t5 =  tot_test2 %>% subset(Product %in% input$pin) %>% 
              group_by(month,year) %>%
              summarise(Total = sum(Total)) %>%  
              pivot_wider(names_from = year, values_from = "Total") %>% 
              mutate(percentage_change = ((`23` - `22`) / `22`) * 100)  
            
            outpre3 = t5 %>% 
              select(1,4)  %>% 
              mutate(
                Year = '23 vs 22',
                percentage_change = if_else(is.na(percentage_change),'-',paste0(round(percentage_change,1),'%')
                ))  %>%
              pivot_wider(names_from = month,values_from = percentage_change)
            
            
            
            MTD = paste0(round(novpc[8,4],1),'%')
            
            cbind(outpre3,MTD)
            
          }
        })
      }
    
  }
   
  ### Tables for Year On Year
  {
    ## sales table
  output$yoydata = renderTable(yoy3dat() ,striped = T,hover = T,digits = 0,na = '-',colnames = T,server = T,width = '100%',align = 'r')
    
    ## variance table
    output$yoyvar = renderTable(yoyvardat3(),striped = T,hover = T,digits = 1,na = '-',colnames = T,server = T,width = '100%',align = 'r',)
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
    
    
    yoytot1 = reactive({
      if(("All" %in% input$pgroupin ) ){
        tot_test2 %>% 
          group_by(year) %>% 
          reframe(Total = sum(Total))
        
      } else {
        tot_test2 %>% 
          subset(ProductGroup %in% input$pgroupin) %>% 
          group_by(year) %>% 
          summarise(Total = sum(Total))
      }
    })
  }
    
    ## Product functionality
    {
      yoytot2 = reactive({
        if( ("All" %in% input$pin)){
          yoytot1()
        } else {
          tot_test2 %>% 
            subset(Product %in% input$pin) %>% 
            group_by(year) %>% 
            summarise(Total = sum(Total))
        }
      })
    
    
    
    
    
    
  }
  }
  
  ### Graph for Year on Year
  {
  output$yoy = renderPlotly({
    plot_ly(data = yoy2(), x = ~month, y = ~Total, type = "bar", color = ~year,
            text = ~paste0('GHs ', round(Total/1000,1),'k\n','20',year,'\n'),
            hoverinfo = text)%>%
      layout(xaxis = list(title = 'Month'), yaxis = list(title = 'Total'))
  })
    
    output$yoytot = renderPlotly({
      plot_ly(data = yoytot2(), x = ~Total, y = ~year, type = "bar", color = ~year,
              text = ~paste0('GHs ', round(Total/1000,1),'k\n','20',year,'\n'),
              hoverinfo = text)%>%
        layout()
    })
  }
  
  ### Reactives for Distribution tab
  {
  DistReac = reactive({
    Preac3() %>% 
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
    ## title for box
    
    output$tptitle = renderText(input$topProduct)
    output$tptitlegraph = renderText(input$topProduct)
    
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
      layout()
  })
  }
  
  ##Total and Quantity tables
  {
  output$topProductsTableTotal = renderTable(TPreactabletotal(),na = '-',width = '100%',align = 'r')
  output$topProductsTableQuantity = renderTable(TPreactablequantity(),na = '-',width = '100%',align = 'r')
  }
  }
  
  ### Reactives for Insights
  {
    HiSa = reactive({
      if('All' %in% input$pgroupin){
      tot_test2 %>% group_by(Product) %>% 
        summarise(Total = sum(Total)) %>%arrange(desc(Total)) %>%  head(1)
      } else {
        tot_test2  %>% group_by(Product) %>% subset(ProductGroup %in% input$pgroupin) %>% summarise(Total = sum(Total)) %>% 
           arrange(desc(Total)) %>%  head(1) 
      }
    })
    
    ExMo = reactive({
      if('All' %in% input$pgroupin){
        tot_test2 %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>%arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      } else {
        tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>% arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      }
    })
    
   
      
    
    ExSt = reactive({
      if('All' %in% input$pgroupin){
        tot_test2 %>% 
          filter(year == 23) %>% filter(month %in% 'Nov') %>% 
          arrange(desc(Quantity)) %>% 
          mutate(ExQuantity =round( Quantity * 1.25,0)) %>% 
          select(2,8)
      } else {
        tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          filter(year == 23) %>%filter(month %in% 'Nov') %>% 
           mutate(ExQuantity =round( Quantity * 1.25,0)) %>% select(2,8)
      }
    })
    
    ExStPy = reactive({
      if('All' %in% input$ptype){
        ExSt()
      } else {
        ExSt() %>% 
          subset(Product %in% tot_test2$Product[
            grepl(paste(input$ptype,collapse='|'), 
                  tot_test2$Product,
                  ignore.case = T)]) 
      }
    })
    
    AvgSaPDData = reactive({
      if('All' %in% input$pgroupin){
      left_join(tot_test2 %>% 
        group_by(month,year) %>% 
        summarise(Total = round(sum(Total)/26,0) )%>% 
        pivot_wider(names_from = month,values_from = Total),tot_test2 %>% 
          group_by(year) %>% 
          summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0) ),by = 'year')
      } else {
        left_join(tot_test2 %>% subset( ProductGroup %in% input$pgroupin) %>% 
                    group_by(month,year) %>% 
                    summarise(Total = round(sum(Total)/26,0) )%>% 
                    pivot_wider(names_from = month,values_from = Total),
                  tot_test2  %>%
                  subset( ProductGroup %in% input$pgroupin) %>% 
                    group_by(year) %>% 
                    summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0) ),by = 'year')
      }
    })
    
    AvgSaPDData2 = reactive({
      if('All' %in% input$pin){
        AvgSaPDData()
      } else {
        left_join(tot_test2 %>% subset( Product %in% input$pin) %>% 
                    group_by(month,year) %>% 
                    summarise(Total = round(sum(Total)/26,0) )%>% 
                    pivot_wider(names_from = month,values_from = Total),
                  tot_test2  %>%
                    subset( Product %in% input$pin) %>% 
                    group_by(year) %>% 
                    summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0) ),by = 'year')
      }
    })
    
  }
  
  ### text for Insights
  {
    ## high sales
    output$HiSa = renderText(paste('Our highest selling product in',if('All' %in% input$pgroupin) 'All Product Groups' else input$pgroupin,'is',as.vector(HiSa()$Product),'with',dollar_format(prefix = 'GHs')(HiSa()$Total),'\n'))
    
    ## highest month
    output$ExMo = renderText(paste('Expected highest month is',as.vector(ExMo()$month),'\nWe expect to sell',dollar_format(prefix = 'GHs')(ExMo()$ExTotal),if('All' %in% input$pgroupin) '' else paste('of', input$pgroupin)))
    
    ## required stocks to sell thatmuch
    output$ExSt = renderText(paste('Required Stocks to meet planned sales for',as.vector(ExMo()$month)))
    
    output$ExStTable = renderDataTable(ExStPy())
    
    output$AvgSaPD = renderTable(AvgSaPDData2(),rownames = F,na = "-",digits = 0,width = '100%',align = 'r')
    
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
  
  pntit = reactive({
    
    paste('Top',input$pnum)
  })
  output$pnumtitle = renderText(pntit())
  
}