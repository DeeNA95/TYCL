source('preprocessing2.R')
source('packages.R')

server <- function(input, output, session) {
  
  ### Reactives for Data and Products
  
  ## Initialising for with year specified
  
  Preac1 = reactive({
    if('All' %in% input$yearnum){
      sales %>% 
      group_by(Name,Product_Group) %>% 
      summarise(Total = round(sum(Total),2),Quantity = round(sum(Quantity),2)) %>% 
      arrange(desc(Total))
    } else {
    sales %>% 
      group_by(Name,Product_Group) %>% 
      subset(year == input$yearnum) %>% 
      summarise(Total = round(sum(Total),2),Quantity = round(sum(Quantity),2))%>% 
      arrange(desc(Total))
  }
})
  
  
  ## Product Group functionality
  
  Preac4 = reactive({
    if ("All" %in% input$pgroupin || is.null(input$pgroupin)) {
      Preac1()
    } else{
      subset(Preac1(), ProductGroup %in% input$pgroupin)
    }
  })
  
  ## High Quantity functionality
  
  Preac5 = reactive({
    if (T == input$highquantity) {
      Preac4() %>% 
      filter(Quantity > input$minquant)
    } else {
      Preac4()
    }
  })
  

  ## Product functionality
  
  Preac6 = reactive({
    if('All' %in% input$pin || is.null(input$pin)){
    Preac5()
    } else{
      subset(Preac5(),Product %in% input$pin)
    }
  })
  
  
  ## Underperforming functionality
  
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
  
  
  ## Product Type functionality
  
  Preac8 = reactive({
    if("All" %in% input$ptype || is.null(input$ptype)){
      Preac7()
    } else{
      subset(Preac7(),
       Product %in%
       sales$Product[grepl('Lele Rice 25kg', 
                              salesduct,
                              ignore.case = TRUE)])
    }
  })
  
  
  
  ### Graph For Products
  
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
  
    
  ### Data tab table
  
  output$pout = renderDT(
    datatable(Preac8(),
              options = list(pageLength = 20),
              rownames = F),
    server = T )
  
  
  ### Reactives for Year on Year Table
  
  ## Year on Year initialisation
    
    yoy1dat = reactive({

      main = sales %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(round(Total,0))) %>% 
        mutate(Total = comma_format()(Total))  %>% 
        pivot_wider(names_from = month,
                    values_from = c("Total"),
                    names_sep = "_") 

  

 tot = sales %>% 
        group_by(year) %>% arrange(desc(year)) %>%
        summarise(Total = sum(round(Total,0))) %>% 
        mutate(Total = comma_format()(Total)) 
  
 maintot = left_join(main,tot, by = 'year')

ytd = sales %>%
  group_by(year, month) %>%
  summarise(Total = sum(round(Total, 0))) %>%
  arrange(month, desc(year))  %>% 
  mutate(YTD = cumsum(Total), YTD = comma_format()(coalesce(YTD, 0))) %>%
  select(year, month, YTD) %>% 
  pivot_wider(
    names_from = month,
    values_from = c("YTD"),
    names_sep = "_"
  ) %>% 
  select(Dec)## increment by one as the months go by ## find a better way to do that though

  left_join(maintot,ytd,by = 'year' ,suffix = c('','YTD'))

    })
  
  ## Product Group functionality
    
    yoy2dat = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
        yoy1dat()
      } else{
       main = sales %>% subset(ProductGroup %in% input$pgroupin) %>%
        group_by(month,year) %>% 
        summarise(Total = sum(round(Total,0))) %>% 
        mutate(Total = comma_format()(Total))  %>% 
        pivot_wider(names_from = month,
                    values_from = c("Total"),
                    names_sep = "_") 

  

 tot = sales %>% subset(ProductGroup %in% input$pgroupin) %>%
        group_by(year) %>% arrange(desc(year)) %>%
        summarise(Total = sum(round(Total,0))) %>% 
        mutate(Total = comma_format()(Total)) 
  
 maintot = left_join(main,tot, by = 'year')

ytd = sales %>% subset(ProductGroup %in% input$pgroupin) %>%
  group_by(year, month) %>%
  summarise(Total = sum(round(Total, 0))) %>%
  arrange(month, desc(year))  %>% 
  mutate(YTD = cumsum(Total), YTD = comma_format()(coalesce(YTD, 0))) %>%
  select(year, month, YTD) %>% 
  pivot_wider(
    names_from = month,
    values_from = c("YTD"),
    names_sep = "_"
  ) %>% 
  select(Dec)## increment by one as the months go by ## find a better way to do that though

  left_join(maintot,ytd,by = 'year' ,suffix = c('','YTD'))
      }
    })
  
  ## Product Functionality
  
    yoy3dat = reactive({
      if('All' %in% input$pin || is.null(input$pin)){
        yoy2dat()
      } else {
        main = sales %>%  subset(Product %in% input$pin)%>%
        group_by(month,year) %>% 
        summarise(Total = sum(round(Total,0))) %>% 
        mutate(Total = comma_format()(Total))  %>% 
        pivot_wider(names_from = month,
                    values_from = c("Total"),
                    names_sep = "_") 

  

 tot = sales %>%  subset(Product %in% input$pin)%>%
        group_by(year) %>% arrange(desc(year)) %>%
        summarise(Total = sum(round(Total,0))) %>% 
        mutate(Total = comma_format()(Total)) 
  
 maintot = left_join(main,tot, by = 'year')

ytd = sales %>% subset(Product %in% input$pin)%>%
  group_by(year, month) %>%
  summarise(Total = sum(round(Total, 0))) %>%
  arrange(month, desc(year))  %>% 
  mutate(YTD = cumsum(Total), YTD = comma_format()(coalesce(YTD, 0))) %>%
  select(year, month, YTD) %>% 
  pivot_wider(
    names_from = month,
    values_from = c("YTD"),
    names_sep = "_"
  ) %>% 
  select(Dec)## increment by one as the months go by ## find a better way to do that though

  left_join(maintot,ytd,by = 'year' ,suffix = c('','YTD'))
    }
      })

  
### Reactives for Year on Year Variance Table

  ## initialise
yoyvardat = reactive({

# Calculate Total and Percentage Change
t1 <- sales %>%  
  group_by(month, year) %>%
  summarise(Total = sum(Total)) %>%
  spread(key = year, value = Total) %>%
  mutate(percentage_change = ((`23` - `22`) / `22`) * 100) %>%
  select(1, 4) %>%
  mutate(
    Year = '23 vs 22',
    percentage_change = if_else(is.na(percentage_change), '-', paste0(round(percentage_change, 1), '%'))
  ) %>%
  pivot_wider(names_from = month, values_from = percentage_change)

# Calculate Month-to-Date (YTD) and Percentage Change
novpc <- sales
  group_by(year, month) %>%
  summarise(Total = sum(Total)) %>%
  mutate(YTD = cumsum(Total)) %>%
  pivot_wider(names_from = year, values_from = c("Total", 'YTD')) %>%
  mutate('YTD %' = (YTD_23 / YTD_22 - 1) * 100) %>%
  select(1, 4, 5, 6)

# Extract YTD percentage for later use
YTD <- paste0(round(novpc[8, 4], 1), '%')

# Combine the output
cbind(t1, YTD)
})

  ## product group functionality
yoyvardat2 =reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
        yoyvardat()
      } else {
        # Calculate Total and Percentage Change
t1 <- sales %>%
  subset(ProductGroup %in% input$pgroupin) %>% 
  group_by(month, year) %>%
  summarise(Total = sum(Total)) %>%
  spread(key = year, value = Total) %>%
  mutate(percentage_change = ((`23` - `22`) / `22`) * 100) %>%
  select(1, 4) %>%
  mutate(
    Year = '23 vs 22',
    percentage_change = if_else(is.na(percentage_change), '-', paste0(round(percentage_change, 1), '%'))
  ) %>%
  pivot_wider(names_from = month, values_from = percentage_change)

# Calculate Month-to-Date (YTD) and Percentage Change
novpc <- sales %>%
  subset(ProductGroup %in% input$pgroupin) %>% 
  group_by(year, month) %>%
  summarise(Total = sum(Total)) %>%
  mutate(YTD = cumsum(Total)) %>%
  pivot_wider(names_from = year, values_from = c("Total", 'YTD')) %>%
  mutate('YTD %' = (YTD_23 / YTD_22 - 1) * 100) %>%
  select(1, 4, 5, 6)

# Extract YTD percentage for later use
YTD <- paste0(round(novpc[8, 4], 1), '%')

# Combine the output
cbind(t1, YTD)
      }
})
     
 ## product functionality    
yoyvardat3 = reactive({
          if('All' %in% input$pin || is.null(input$pin)){
            yoyvardat2()
          } else{
             # Calculate Total and Percentage Change
t1 <- sales
  subset(Product %in% input$pin) %>% 
  group_by(month, year) %>%
  summarise(Total = sum(Total)) %>%
  spread(key = year, value = Total) %>%
  mutate(percentage_change = ((`23` - `22`) / `22`) * 100) %>%
  select(1, 4) %>%
  mutate(
    Year = '23 vs 22',
    percentage_change = if_else(is.na(percentage_change), '-', paste0(round(percentage_change, 1), '%'))
  ) %>%
  pivot_wider(names_from = month, values_from = percentage_change)

# Calculate Month-to-Date (YTD) and Percentage Change
novpc <- sales %>%
  subset(Product %in% input$pin) %>% 
  group_by(year, month) %>%
  summarise(Total = sum(Total)) %>%
  mutate(YTD = cumsum(Total)) %>%
  pivot_wider(names_from = year, values_from = c("Total", 'YTD')) %>%
  mutate('YTD %' = (YTD_23 / YTD_22 - 1) * 100) %>%
  select(1, 4, 5, 6)

# Extract YTD percentage for later use
YTD <- paste0(round(novpc[8, 4], 1), '%')

# Combine the output
cbind(t1, YTD)
          }
})
   
  ### Tables for Year On Year
  
    ## sales table
  output$yoydata = renderTable(yoy3dat(),hover = T,digits = 0,na = '-',colnames = T,server = F,align = 'r',spacing = 'xs',width = '100%')
    
    ## variance table
  output$yoyvar = renderTable(yoyvardat3(),hover = T,digits = 1,na = '-',colnames = T,server = T,align = 'r',spacing = 'xs',width = '100%')
  
  
  ### Reactives for Year On Year Graph
  
  ## Initialise and Product Group functionality
  
  yoy1 = reactive({
    if(("All" %in% input$pgroupin || is.null(input$pgroupin)) ){
    sales %>% 
      group_by(month,year) %>% 
        summarise(Total = sum(Total))
      
    } else {
      sales %>% 
        subset(ProductGroup %in% input$pgroupin) %>% 
      group_by(month,year) %>% 
        summarise(Total = sum(Total))
    }
  })
  
  ## Product functionality
  
  yoy2 = reactive({
    if( ("All" %in% input$pin || is.null(input$pin))){
      yoy1()
    } else {
      sales %>% 
        subset(Product %in% input$pin) %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(Total))
    }
  })
    
  yoytot1 = reactive({
      if(("All" %in% input$pgroupin || is.null(input$pgroupin) ) ){
        sales %>% 
          group_by(year) %>% 
          reframe(Total = sum(Total))
        
      } else {
        sales %>% 
          subset(ProductGroup %in% input$pgroupin) %>% 
          group_by(year) %>% 
          summarise(Total = sum(Total))
      }
  })
  
    ## Product functionality
    
  yoytot2 = reactive({
        if( ("All" %in% input$pin || is.null(input$pin))){
          yoytot1()
        } else {
          sales %>% 
            subset(Product %in% input$pin) %>% 
            group_by(year) %>% 
            summarise(Total = sum(Total))
        }
  })
    
    
  
  ### Graph for Year on Year
  
  output$yoy = renderPlotly({
    plot_ly(data = yoy2(), x = ~month, y = ~Total, type = "bar", color = ~year,
            text = ~paste0('GHs ', round(Total/1000,1),'k\n','20',year,'\n'),
            hoverinfo = text,
            colors = c('orange','#8c8099'
            #,'#7aba77'
            )
            )%>%
      layout(xaxis = list(title = 'Month'), yaxis = list(title = 'Total'))
  })
    
  output$yoytot = renderPlotly({
      plot_ly(data = yoytot2(), x = ~Total, y = ~year, type = "bar", color = ~year,
              text = ~paste0('GHs ', round(Total/1000,1),'k\n','20',year,'\n'),
              hoverinfo = text,
              colors = c('orange','#8c8099'
              #,'#7aba77'
              )
              )%>%
        layout()
  })
  
  
  ### Reactives for Distribution tab
  
  DistReac = reactive({
    Preac1() %>%
      mutate(category = if_else(ProductGroup %in% minor_pgroups,'Others',ProductGroup),
      category = if_else(ProductGroup %in% external_pgroups,'Externals',category))  %>% 
      group_by(category) %>% 
      summarise(Total = sum(Total))
  })
  
  
  ### Graph for Distribution tab
  
  output$pie = renderPlotly({
    plot_ly(DistReac(),
            type = 'pie',
            labels = ~category,
            values = ~Total
            ) %>%
      layout(title = paste0( if('All' %in% input$yearnum) paste('All years') else paste0('20',input$yearnum)))
  })
  
  
  ### Reactives for Top Products tab

    ## title for box
    
    output$tptitle = renderText(input$topProduct)
    output$tptitlegraph = renderText(input$topProduct)
    
  ## Initialise and filter functionality for Graph
  
  TPreacGraph = reactive({
    sales %>% filter(Product %in% input$topProduct) %>% 
      group_by(month,year) %>% 
      summarise(Total = sum(Total))
  })
  
  
  ## Initialise and filter for total table
  
  TPreactabletotal = shiny::reactive({
    left_join(sales %>% filter(Product %in% input$topProduct) %>% 
      group_by(month,year) %>% 
      summarise(Total = sum(Total)) %>% 
        mutate(Total = comma_format()(as.numeric(Total))) %>% 
      pivot_wider(names_from = month,values_from = Total),
      sales %>% filter(Product %in% input$topProduct) %>% 
        group_by(year) %>% 
        summarise(Total = sum(Total)) %>% 
        mutate(Total = comma_format()(as.numeric(Total))), by = 'year')
  })
  
  ## Initialise and filter for quantity table
  
  TPreactablequantity = reactive({
    left_join(sales %>% filter(Product %in% input$topProduct) %>% 
                group_by(month,year) %>% 
                summarise(Quantity = sum(Quantity)) %>% 
                mutate(Quantity = comma_format()(as.numeric(Quantity))) %>% 
                pivot_wider(names_from = month,values_from = Quantity),
              sales %>% filter(Product %in% input$topProduct) %>% 
                group_by(year) %>% 
              summarise(Quantity = sum(Quantity)) %>% 
                mutate(Quantity = comma_format()(as.numeric(Quantity))), by = 'year')
  })
  
  ## Graph for Top Products
  
  output$topProductGraph = renderPlotly({
    plot_ly(TPreacGraph(),
            x = ~month,
            y = ~Total,
            type = 'bar',
            color = ~year,
            colors = c('orange','#8c8099'
              #,'#7aba77'
              )
              ) %>% 
      layout()
  })
  
  ##Total and Quantity tables
  
  output$topProductsTableTotal = renderTable(TPreactabletotal(),na = '-',width = '100%',align = 'r')
  output$topProductsTableQuantity = renderTable(TPreactablequantity(),na = '-',width = '100%',align = 'r')
  
  
  
  ### Reactives for Insights
  
    HiSa = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
      sales %>% group_by(Product) %>% 
        summarise(Total = sum(Total)) %>%arrange(desc(Total)) %>%  head(1)
      } else {
        sales  %>% group_by(Product) %>% subset(ProductGroup %in% input$pgroupin) %>% summarise(Total = sum(Total)) %>% 
           arrange(desc(Total)) %>%  head(1) 
      }
    })
    
    ExMo = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
        sales %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>%arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      } else {
        sales %>% subset(ProductGroup %in% input$pgroupin) %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>% arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      }
    })
    
    ExSt = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
        sales %>% 
          filter(year == 23) %>% filter(month %in% 'Nov') %>% 
          arrange(desc(Quantity)) %>% 
          mutate(ExQuantity =round( Quantity * 1.25,0)) %>% 
          select(2,8)
      } else {
        sales %>% subset(ProductGroup %in% input$pgroupin) %>% 
          filter(year == 23) %>%filter(month %in% 'Nov') %>% 
           mutate(ExQuantity =round( Quantity * 1.25,0))%>%
          arrange(desc(ExQuantity) ) %>% select(2,8)
      }
    })
    
    ExStPy = reactive({
      if('All' %in% input$ptype || is.null(input$ptype)){
        ExSt()
      } else {
        ExSt() %>% 
          subset(Product %in% sales$Product[
            grepl(paste(input$ptype,collapse='|'), 
                  sales$Product,
                  ignore.case = T)]) %>%
          arrange(desc(ExQuantity) )
      }
    })
    
    AvgSaPDData = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
      left_join(sales %>% 
        group_by(month,year) %>% 
        summarise(Total = round(sum(Total)/26,0)) %>% 
        mutate(Total = comma_format()(Total))%>% 
        pivot_wider(names_from = month,values_from = Total),
        sales %>% 
          group_by(year) %>% 
          summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48),0))%>% 
        mutate("Year Average" = comma_format()(`Year Average`)) ,by = 'year')
      } else {
        left_join(sales %>% subset( ProductGroup %in% input$pgroupin) %>% 
                    group_by(month,year) %>% 
                    summarise(Total = round(sum(Total)/26,0) )%>% 
        mutate(Total = comma_format()(Total))%>% 
                    pivot_wider(names_from = month,values_from = Total) ,
                  sales  %>%
                  subset( ProductGroup %in% input$pgroupin) %>% 
                    group_by(year) %>% 
                    summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0))%>% 
        mutate("Year Average" = comma_format()(`Year Average`)) ,by = 'year')
      }
    })
    
    AvgSaPDData2 = reactive({
      if('All' %in% input$pin || is.null(input$pin)){
        AvgSaPDData()
      } else {
        left_join(sales %>% subset( Product %in% input$pin) %>% 
                    group_by(month,year) %>% 
                    summarise(Total = round(sum(Total)/26,0) ) %>% 
        mutate(Total = comma_format()(Total))%>% 
                    pivot_wider(names_from = month,values_from = Total),
                  sales  %>%
                    subset( Product %in% input$pin) %>% 
                    group_by(year) %>% 
                    summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0))%>% 
        mutate("Year Average" = comma_format()(`Year Average`)) ,
                  by = 'year')
      }
    })

    AvgSaPDData3 = reactive({
      if('All' %in% input$ptype || is.null(input$ptype)){
        AvgSaPDData2()
      } else {
        left_join(sales %>%  subset( 
             Product %in% 
               sales$Product[
                 grepl( paste(input$ptype,collapse='|'), 
                       sales$Product,
                       ignore.case = T)]) %>% 
                    group_by(month,year) %>% 
                    summarise(Total = round(sum(Total)/26,0) ) %>% 
        mutate(Total = comma_format()(Total))%>% 
                    pivot_wider(names_from = month,values_from = Total),
                  sales  %>%
                     subset(
             Product %in% 
               sales$Product[
                 grepl( paste(input$ptype,collapse='|'), 
                       sales$Product,
                       ignore.case = T)]) %>% 
                    group_by(year) %>% 
                    summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0))%>% 
        mutate("Year Average" = comma_format()(`Year Average`)) ,
                  by = 'year')
      }
    })

    output$AvgSaPD = renderTable(AvgSaPDData3(),rownames = F,na = "-",digits = 0,width = '100%',align = 'r')
    

  ### text for Insights
  
    ## high sales
    output$HiSa = renderText(
      paste('Our highest selling product in',
      if('All' %in% input$pgroupin || is.null(input$pgroupin)) 'All Product Groups' else input$pgroupin,
      'is',
      as.vector(HiSa()$Product),
      'with',
      dollar_format(prefix = 'GHs')(HiSa()$Total),
      '\n')
      )
    
    ## highest month
    output$ExMo = renderText(
      paste('Expected highest month is',
      as.vector(ExMo()$month),
      '\nWe expect to sell',
      dollar_format(prefix = 'GHs')(ExMo()$ExTotal),
      if('All' %in% input$pgroupin|| is.null(input$pgroupin)) '' else paste('of', input$pgroupin))
      )
    
    ## required stocks to sell thatmuch
    output$ExSt = renderText(
      paste('Required Stocks to meet planned sales for',
      as.vector(ExMo()$month))
      )
    
    output$ExStTable = DT::renderDataTable(ExStPy())


    ## prediction for 24

    p241 = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin) ){
        main = predict24 %>% group_by(month) %>% summarise( Total = sum(ExTotal)) %>% 
        mutate(Total = comma_format()(Total))  %>% 
        pivot_wider(names_from = month, values_from = Total)

        tot = predict24 %>% summarise( Total = sum(ExTotal))  %>% 
        mutate(Total = comma_format()(Total))

        cbind(main,tot)
      } else {
        main = predict24 %>% filter(ProductGroup %in% input$pgroupin) %>% group_by(month) %>% summarise( Total = sum(ExTotal)) %>% 
        mutate(Total = comma_format()(Total))  %>% 
        pivot_wider(names_from = month, values_from = Total)

        tot = predict24 %>% filter(ProductGroup %in% input$pgroupin) %>% summarise( Total = sum(ExTotal)) %>% 
        mutate(Total = comma_format()(Total))

        cbind(main,tot)
      }
    })

    p242 = reactive({
      if('All' %in% input$ptype|| is.null(input$ptype) ){
        p241()
      } else {
        main = predict24 %>% subset(
             Product %in% 
               sales$Product[
                 grepl( paste(input$ptype,collapse='|'), 
                       sales$Product,
                       ignore.case = T)]) %>% group_by(month) %>% summarise( Total = sum(ExTotal)) %>% 
        mutate(Total = comma_format()(Total))  %>% 
        pivot_wider(names_from = month, values_from = Total)

        tot = predict24 %>% subset(
             Product %in% 
               sales$Product[
                 grepl( paste(input$ptype,collapse='|'), 
                       sales$Product,
                       ignore.case = T)]) %>% summarise( Total = sum(ExTotal)) %>% 
        mutate(Total = comma_format()(Total))

        cbind(main,tot)
      }
    })
    
    p243 = reactive({
      if('All' %in% input$pin|| is.null(input$pin) ){
        p242()
      } else {
       main = predict24 %>% subset(Product %in% input$pin) %>% group_by(month) %>% summarise( Total = sum(ExTotal)) %>% 
        mutate(Total = comma_format()(Total))  %>% 
        pivot_wider(names_from = month, values_from = Total)

        tot = predict24 %>% subset(Product %in% input$pin) %>% summarise( Total = sum(ExTotal)) %>% 
        mutate(Total = comma_format()(Total))

        cbind(main,tot)
      }
    })
    
    output$p24 = renderTable(p243(), width = '100%')
  
  ### cross section
  crossby1 = reactive({
    if('All' %in% input$yearnum2){
      sales %>% group_by(month,year, ProductGroup) %>% summarise(Total = sum(Total))
    } else {
      sales %>% group_by(month,year, ProductGroup)%>% filter(year == input$yearnum2) %>% summarise(Total = sum(Total))
    }
  })

  crossbyplot = reactive({
    if('Product Group' %in% input$cp){
      p = ggplot(crossby1(), aes(month, Total, fill = year)) + 
      facet_wrap(~ProductGroup, scales = 'free_y', nrow = 5, shrink = T,as.table = F) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
             panel.grid.major.y = element_line(color = 'grey'),  # Set color for horizontal gridlines
             panel.grid.major.x = element_blank(),  # Remove vertical gridlines
             panel.grid.minor = element_blank(),  # Remove minor gridlines
             panel.background = element_rect(fill = 'white'),  # Set background color
             plot.background = element_rect(fill = 'white'),
             strip.text = element_text(color = 'black'),  # Set color for strip text
             strip.background = element_rect(fill = 'white'),  # Set background color for the strip
             panel.spacing = unit(0.5, "lines")
              )+
      scale_fill_manual(values = c('orange','#8c8099','#7aba77'))


      ggplotly(p, height = 820) 
    } else {
      p = ggplot(crossby1(), aes(ProductGroup, Total, fill = year)) +
       facet_wrap(~month, scales = 'free_y',nrow = 5, shrink = T, as.table = F,strip.position = 'right') + 
       geom_col() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_fill_manual(values = c('orange','#8c8099','#7aba77')) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
             panel.grid.major.y = element_line(color = 'grey'),  # Set color for horizontal gridlines
             panel.grid.major.x = element_blank(),  # Remove vertical gridlines
             panel.grid.minor = element_blank(),  # Remove minor gridlines
             panel.background = element_rect(fill = 'white'),  # Set background color
             plot.background = element_rect(fill = 'white'),
             strip.text = element_text(color = 'black'),  # Set color for strip text
             strip.background = element_rect(fill = 'white'),  # Set background color for the strip
             panel.spacing = unit(0.5, "lines") )+
      scale_y_log10( )+
      labs(title = 'This is not to scale. It is plotted against a log scale to adequately visualise diferences among minor product groups')


      ggplotly(p,height = 820)  

    }
  })

  
  output$cross1 = renderPlotly(crossbyplot())



### Month analysis

monAnalTot = reactive({
  if('All' != input$yearnum || is.null(input$yearnum)){
  sales %>%
  filter(month %in% input$month, year == input$yearnum) %>%
  group_by(ProductGroup)%>%
  summarise(Total = sum(Total)) %>%
  mutate(Total = comma_format()(Total)) %>% 
  pivot_wider(names_from = ProductGroup, values_from = c(Total))
  } else {
    sales  %>%
    filter(month %in% input$month)%>%
  group_by(ProductGroup)%>%
  summarise(Total = sum(Total)) %>%
  mutate(Total = comma_format()(Total)) %>% 
  pivot_wider(names_from = ProductGroup, values_from = c(Total))
  }
})

monAnalQ = reactive({
  if('All' != input$yearnum || is.null(input$yearnum)){
  sales %>%
  filter(month %in% input$month, year == input$yearnum) %>%
  group_by(ProductGroup)%>%
  summarise( Quantity = sum(Quantity)) %>%
  mutate(Quantity = comma_format()(Quantity)) %>% 
  pivot_wider(names_from = ProductGroup, values_from = c(Quantity))
  } else {
    sales  %>%
    filter(month %in% input$month)%>%
  group_by(ProductGroup)%>%
  summarise( Quantity = sum(Quantity)) %>%
  mutate(Quantity = comma_format()(Quantity)) %>% 
  pivot_wider(names_from = ProductGroup, values_from = c(Quantity))
  }
})

monAnalVar = reactive({
  if(input$yearnum == 'All'){
    print('Viewing All')
  } else if (length(input$month) > 1){
    print('Viewing Multiple Months')
  } else if(input$yearnum == 22){
    print('No Past Data')
  }  else if (input$month %in% c('Jan','Feb','Mar')){
print('No Past Data')
    } else {
  sales %>%  subset(month %in% as.vector(input$month)) %>%
  group_by(year, ProductGroup) %>%
  summarise(Total = sum(Total)) %>%
  spread(key = year, value = Total) %>%
  mutate(percentage_change = ((`23` - `22`) / `22`) * 100) %>%
  select(1, 4) %>%
  mutate(
    Year = 'vs PY',
    percentage_change = if_else(is.na(percentage_change), '-', paste0(round(percentage_change, 1), '%'))
  ) %>%
  pivot_wider(names_from = ProductGroup, values_from = percentage_change)
  }


  

})

output$monanaltableT = renderTable(monAnalTot())
output$monanaltableQ = renderTable(monAnalQ())
output$varmonth = renderTable(monAnalVar(), spacing = 'xs')
## top month products

tpmonth = reactive({
  if('All' != input$yearnum){
  sales %>%
  group_by(Product,ProductGroup)%>%
  filter(month %in% input$month, year == input$yearnum) %>%
  summarise(Total = sum(Total), Quantity = sum(Quantity)) %>%
  arrange(desc(Total)) %>% head(input$pnum2)
  } else {
    sales %>%
  group_by(Product,ProductGroup)%>%
  filter(month %in% input$month) %>%
  summarise(Total = sum(Total), Quantity = sum(Quantity)) %>%
  arrange(desc(Total)) %>% head(input$pnum2)
  } 
})



output$tpmonthgraph = renderPlotly({
  plot_ly(
    tpmonth(),
    y = ~reorder(Product,desc(Total)),
    x = ~Total,
    type = 'bar',
    color = ~ProductGroup,
    colors =  c('orange','#8c8099','#7aba77','#77b4ba','#aeba77')
  ) %>% layout(
    yaxis = list(title = 'Product'),
    xaxis = list(title = 'Total')
  )
})

tpmonthpiedat = reactive({
   if('All' != input$yearnum){
  sales %>% mutate(category = if_else(ProductGroup %in% minor_pgroups,'Others',ProductGroup),
      category = if_else(ProductGroup %in% external_pgroups,'Externals',category))  %>% 
      group_by(category) %>%
  filter(month %in% input$month, year == input$yearnum) %>%
  summarise(Total = sum(Total)) %>%
  arrange(desc(Total))
  } else {
    sales %>% mutate(category = if_else(ProductGroup %in% minor_pgroups,'Others',ProductGroup),
      category = if_else(ProductGroup %in% external_pgroups,'Externals',category))  %>% 
      group_by(category) %>%
  filter(month %in% input$month) %>%
  summarise(Total = sum(Total)) %>%
  arrange(desc(Total))
  } 
})

output$monthpie = renderPlotly({
  plot_ly(
    tpmonthpiedat(),
    type = 'pie',
            labels = ~category,
            values = ~Total
            ) %>%
      layout(title = paste0( if('All' %in% input$yearnum) paste('All years') else paste0('20',input$yearnum)))
})


output$monthtit = renderText({
  if(length(input$month) > 1){
  
  } else {
    paste(input$month,input$yearnum)
  }
})










  ### Server side select/selectize
  
  ##server side select for yearnum
    
  updateSelectizeInput(session, 'yearnum', choices = c('All', `Years` = list(sales$year)), server = TRUE,selected = 23)
  updateSelectizeInput(session, 'yearnum2', choices = c('All', `Years` = list(sales$year)), server = TRUE,selected = 'All')  
    
  ## server side select for product
    
  updateSelectInput(session, 'pin', choices = c('All', `Products` = list(products$Name)),selected = NULL)
    
  
  
  ### Observes
  
  ## Observe for changing product options when product group is selected
  
  observe({
    product_choice = subset(products, ProductGroup %in% input$pgroupin)
    updateSelectInput(inputId = 'pin',choices = c('All',product_choice$Name),selected = NULL)
  })

   
   # Show a 30s message when entering the YOY tab

   welcome_message_count <- reactiveVal(0)

   observe({
  if (!is.null(input$tabs)) {
    selected_tab <- input$tabs
    if (selected_tab == 'PM' && welcome_message_count() < 3) {
      showModal(
        modalDialog(
          'Scroll Down For More Information On The Page',
          h6(
            'To change from \'All\', click on \'All\' and then click delete or backspace'
          ),
          footer = tagList(
            actionButton("closeBtn", "Close")
          )
        )
      )
      # Increment the count
      welcome_message_count(welcome_message_count() + 1)
    }
  }
})

  # Close the modal when the close button is clicked
  observeEvent(input$closeBtn, {
    removeModal()
  })

  pntit = reactive({
    paste('Top',input$pnum)
  })
  output$pnumtitle = renderText(pntit())


  }

