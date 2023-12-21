source('preprocessing.R')
source('packages.R')

server <- function(input, output, session) {
  
  ### Reactives for Data and Products
  
  ## Initialising for with year specified
  
  Preac1 = reactive({
    if('All' %in% input$yearnum){
tot_test2 %>% 
      group_by(Product,ProductGroup) %>% 
      summarise(Total = round(sum(Total),2),Quantity = round(sum(Quantity),2)) %>% 
      arrange(desc(Total))
    } else {
    tot_test2 %>% 
      group_by(Product,ProductGroup) %>% 
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
       tot_test2$Product[grepl('Lele Rice 25kg', 
                              tot_test2$Product,
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
      eomnov = tot_test2 %>%
  group_by(year, month) %>%
  summarise(Total = sum(round(Total, 0))) %>%
  arrange(month, year) %>% 
  mutate(YTD = cumsum(Total), YTD = comma_format()(coalesce(YTD, 0))) %>%
  select(year, month, YTD) %>% 
  pivot_wider(
    names_from = month,
    values_from = c("YTD"),
    names_sep = "_"
  ) %>% 
  select(Nov)## increment by one as the months go by ## find a better way to do that though

     left_join(
      tot_test2 %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(round(Total,0))) %>% 
        pivot_wider(names_from = month,
                    values_from = c("Total"),
                    names_sep = "_") %>% rowwise() %>% 
        mutate('Total' = rowSums(pick(where(is.numeric)),na.rm = T)),
      eomnov,
      by = 'year',
      suffix= c('','YTD'))
    })
  
  ## Product Group functionality
    
    yoy2dat = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
        yoy1dat()
      } else{
        eomnov = tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>%
      group_by(year, month) %>%
      arrange(month, year) %>% 
  summarise(Total = sum(round(Total, 0))) %>%
  mutate(YTD = cumsum(Total), YTD = comma_format()(coalesce(YTD, 0))) %>%
  select(year, month, YTD) %>% 
  pivot_wider(
    names_from = month,
    values_from = c("YTD"),
    names_sep = "_"
  ) %>% 
  select(Nov) ## increment by one as the months go by ## find a better way to do that though
        
        left_join(tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(round(Total,0))) %>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_") %>% 
          mutate('Total' = rowSums(pick(where(is.numeric)),na.rm = T)),
      eomnov,
      by = 'year',
      suffix= c('','YTD'))
      }
    })
  
  ## Product Functionality
  
    yoy3dat = reactive({
      if('All' %in% input$pin || is.null(input$pin)){
        yoy2dat()
      } else {
        eomnov = tot_test2 %>% subset(Product %in% input$pin) %>%
      group_by(year, month) %>%
      arrange(month, year) %>% 
  summarise(Total = sum(round(Total, 0))) %>%
  mutate(YTD = cumsum(Total), YTD = comma_format()(coalesce(YTD, 0))) %>%
  select(year, month, YTD) %>% 
  pivot_wider(
    names_from = month,
    values_from = c("YTD"),
    names_sep = "_"
  ) %>% 
  select(Nov) ## increment by one as the months go by ## find a better way to do that though
        
        left_join(tot_test2 %>%  subset(Product %in% input$pin) %>% 
          group_by(month,year) %>% 
          summarise(Total = sum(round(Total,0)) )%>% 
          pivot_wider(names_from = month,
                      values_from = c("Total"),
                      names_sep = "_") %>% 
          mutate('Total' = rowSums(pick(where(is.numeric)),na.rm = T) ),
      eomnov,
      by = 'year',
      suffix= c('','YTD'))
    }
      })

  
### Reactives for Year on Year Variance Table

  ## initialise
yoyvardat = reactive({

# Calculate Total and Percentage Change
t1 <- tot_test2 %>%  
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
novpc <- tot_test2 %>%
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
t1 <- tot_test2 %>%
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
novpc <- tot_test2 %>%
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
t1 <- tot_test2 %>%
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
novpc <- tot_test2 %>%
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
  
  ## Product functionality
  
  yoy2 = reactive({
    if( ("All" %in% input$pin || is.null(input$pin))){
      yoy1()
    } else {
      tot_test2 %>% 
        subset(Product %in% input$pin) %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(Total))
    }
  })
    
  yoytot1 = reactive({
      if(("All" %in% input$pgroupin || is.null(input$pgroupin) ) ){
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
  
    ## Product functionality
    
  yoytot2 = reactive({
        if( ("All" %in% input$pin || is.null(input$pin))){
          yoytot1()
        } else {
          tot_test2 %>% 
            subset(Product %in% input$pin) %>% 
            group_by(year) %>% 
            summarise(Total = sum(Total))
        }
  })
    
    
  
  ### Graph for Year on Year
  
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
    tot_test2 %>% filter(Product %in% input$topProduct) %>% 
      group_by(month,year) %>% 
      summarise(Total = sum(Total))
  })
  
  
  ## Initialise and filter for total table
  
  TPreactabletotal = shiny::reactive({
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
  
  ## Initialise and filter for quantity table
  
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
  
  ## Graph for Top Products
  
  output$topProductGraph = renderPlotly({
    plot_ly(TPreacGraph(),
            x = ~month,
            y = ~Total,
            type = 'bar',
            color = ~year) %>% 
      layout()
  })
  
  ##Total and Quantity tables
  
  output$topProductsTableTotal = renderTable(TPreactabletotal(),na = '-',width = '100%',align = 'r')
  output$topProductsTableQuantity = renderTable(TPreactablequantity(),na = '-',width = '100%',align = 'r')
  
  
  
  ### Reactives for Insights
  
    HiSa = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
      tot_test2 %>% group_by(Product) %>% 
        summarise(Total = sum(Total)) %>%arrange(desc(Total)) %>%  head(1)
      } else {
        tot_test2  %>% group_by(Product) %>% subset(ProductGroup %in% input$pgroupin) %>% summarise(Total = sum(Total)) %>% 
           arrange(desc(Total)) %>%  head(1) 
      }
    })
    
    ExMo = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
        tot_test2 %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>%arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      } else {
        tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
          filter(year == 22)%>% group_by(month) %>%  summarise(Total = sum(Total)) %>% arrange(desc(Total)) %>% mutate(ExTotal = Total*2.5) %>% head(1) %>% select(1,3) 
      }
    })
    
    ExSt = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
        tot_test2 %>% 
          filter(year == 23) %>% filter(month %in% 'Nov') %>% 
          arrange(desc(Quantity)) %>% 
          mutate(ExQuantity =round( Quantity * 1.25,0)) %>% 
          select(2,8)
      } else {
        tot_test2 %>% subset(ProductGroup %in% input$pgroupin) %>% 
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
          subset(Product %in% tot_test2$Product[
            grepl(paste(input$ptype,collapse='|'), 
                  tot_test2$Product,
                  ignore.case = T)]) %>%
          arrange(desc(ExQuantity) )
      }
    })
    
    AvgSaPDData = reactive({
      if('All' %in% input$pgroupin || is.null(input$pgroupin)){
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
      if('All' %in% input$pin || is.null(input$pin)){
        AvgSaPDData()
      } else {
        left_join(tot_test2 %>% subset( Product %in% input$pin) %>% 
                    group_by(month,year) %>% 
                    summarise(Total = round(sum(Total)/26,0) )%>% 
                    pivot_wider(names_from = month,values_from = Total),
                  tot_test2  %>%
                    subset( Product %in% input$pin) %>% 
                    group_by(year) %>% 
                    summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0)),
                  by = 'year')
      }
    })

    AvgSaPDData3 = reactive({
      if('All' %in% input$ptype || is.null(input$ptype)){
        AvgSaPDData2()
      } else {
        left_join(tot_test2 %>%  subset( 
             Product %in% 
               tot_test2$Product[
                 grepl( paste(input$ptype,collapse='|'), 
                       tot_test2$Product,
                       ignore.case = T)]) %>% 
                    group_by(month,year) %>% 
                    summarise(Total = round(sum(Total)/26,0) )%>% 
                    pivot_wider(names_from = month,values_from = Total),
                  tot_test2  %>%
                     subset(
             Product %in% 
               tot_test2$Product[
                 grepl( paste(input$ptype,collapse='|'), 
                       tot_test2$Product,
                       ignore.case = T)]) %>% 
                    group_by(year) %>% 
                    summarise('Year Average' = round(sum(Total,na.rm = T)/(365-48-27),0)),
                  by = 'year')
      }
    })

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
    
    output$ExStTable = DT::renderDataTable(ExStPy(),)
    
    output$AvgSaPD = renderTable(AvgSaPDData3(),rownames = F,na = "-",digits = 0,width = '100%',align = 'r')
    
  
  ### cross section
  crossby1 = reactive({
    if('All' %in% input$yearnum2){
      tot_test2 %>% group_by(month,year, ProductGroup) %>% summarise(Total = sum(Total))
    } else {
      tot_test2 %>% group_by(month,year, ProductGroup)%>% filter(year == input$yearnum2) %>% summarise(Total = sum(Total))
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
             panel.spacing = unit(0.5, "lines") )+
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




  ### Server side select/selectize
  
  ##server side select for yearnum
    
  updateSelectizeInput(session, 'yearnum', choices = c('All', `Years` = list(tot_test2$year)), server = TRUE,selected = 23)
  updateSelectizeInput(session, 'yearnum2', choices = c('All', `Years` = list(tot_test2$year)), server = TRUE,selected = 'All')  
    
  ## server side select for product
    
  updateSelectInput(session, 'pin', choices = c('All', `Products` = list(products$Name)),selected = 'All')
    
  
  
  ### Observes
  
  ## Observe for changing product options when product group is selected
  
  observe({
    product_choice = subset(products, ProductGroup %in% input$pgroupin)
    updateSelectInput(inputId = 'pin',choices = c('All',product_choice$Name),selected = 'All')
  })

   
   # Show a 30s message when entering the YOY tab

   welcome_message_count <- reactiveVal(0)

   observe({
  if (!is.null(input$tabs)) {
    selected_tab <- input$tabs
    if (selected_tab == 'YOY' && welcome_message_count() < 3) {
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