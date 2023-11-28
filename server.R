

source('preprocessing.R')




server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###top products backend
  {
    #layer thighs
    tp1 = reactive({
      left_join(tot_test2 %>% 
        filter(Product %in% 'Chicken Layer Thigh Hard')%>% 
        select(-c(1,2,7)) %>% 
        group_by(month,year) %>% 
        summarise(Total = sum(Total)) %>% 
        mutate(Total = comma_format()(as.numeric(Total))) %>% 
        pivot_wider(names_from = month,values_from = c(Total)),
        tot_test2 %>% 
          filter(Product %in% input$seltp)%>% 
          select(-c(1,2,7)) %>% 
          group_by(year) %>% 
          summarise(Total = sum(Total))%>% 
          mutate(Total = comma_format()(as.numeric(Total))), by = 'year'
      )
    })
    tp2 =reactive({
      left_join(tot_test2 %>% 
        filter(Product %in% input$seltp)%>% 
        select(-c(1,2,7)) %>% 
        group_by(month,year) %>% 
        summarise(Quantity = sum(Quantity)) %>% 
        #mutate(Quantity = comma_format()(as.numeric(Quantity))) %>% 
        pivot_wider(names_from = month,values_from = c(Quantity)),
        tot_test2 %>% 
          filter(Product %in% input$seltp)%>% 
          select(-c(1,2,7)) %>% 
          group_by(year) %>% 
          summarise(Quantity = sum(Quantity))#%>% 
          #mutate(Quantity = comma_format()(as.numeric(Quantity))), by = 'year'
      )
      })
    tp3 = reactive({
      tot_test2 %>% 
        filter(Product %in% input$seltp)%>% 
        select(-c(1,2,7)) %>% 
        group_by(year) 
    })
    
    output$tpoutdat1 = renderTable(tp1())
    output$tpoutdat2 = renderTable(tp2())
    output$tpout = renderPlotly({
      plot_ly(
        tp3(),
        x = ~month,
        y= ~Total,
        type = 'bar',
        color = ~year
      ) %>% 
        layout(title = input$seltp)
    })
    
    
    
    
    
    
    
    
    
    
  }
  
  
  
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
  
  observe({
    product_choice = subset(products, products$ProductGroup %in% input$pgroupin)
    
    
    updateSelectInput(inputId = 'pin',choices = c('All',product_choice$Name),selected = 'All')
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
  
  
  
  
  output$plo = renderPlotly({
    
    
    plot_ly(
      data = head(touf(),10),
      x = ~reorder(Product,desc(if (T == input$highquantity) Quantity else if (T == input$undperf && 'Quantity' == input$undperftype) Quantity else Total)),
      y = ~(if (T == input$highquantity) Quantity else if (T == input$undperf && 'Quantity' == input$undperftype) Quantity else Total),
      type = "bar",
      color = ~ProductGroup,
      text = ~paste0('GHs ', round(Total/1000, 1), 'k', '\nQuantity: ', round(Quantity, 1)),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = 'Products', tickangle = -15, tickmode = 'array'),
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
          }),
        categoryorder = "array",
        categoryarray = unique(touf()$ProductGroup)
      )
    
    
    
  })
    
  ##current year datatable
  output$pout = renderDataTable(touf())
  
  
  ###YoY analysis
  
  #reactives for yoy\
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
    
    
    cname = c('Year',month.name,"Total")
  output$yoydata = renderTable(yoy3dat(),striped = T,hover = T,digits = 1,na = '-',width = 2000,colnames = T)
  
    
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
  
  
  
  #graph for yoy
  {
  output$yoy = renderPlotly({
    
    plot_ly(data = yoy2(), x = ~month, y = ~Total, type = "bar", color = ~year,
            text = ~paste0('GHs ', round(Total/1000,1),'k\n','20',year),
            hoverinfo = text)%>%
      layout(xaxis = list(title = 'Month'), yaxis = list(title = 'Total'))
      
  })
  
  }
  }
  
  
  topie = reactive({
    tout() %>% 
      group_by(ProductGroup) %>% 
      summarise(Total = sum(Total))
  })
  
  output$pie = renderPlotly({
    plot_ly(topie(),type = 'pie',labels = ~ProductGroup,values = ~Total
            ) %>% layout(title = paste0( if('All' %in% input$yearnum) paste('All years') else paste0('20',input$yearnum)))
  })
  
  
  
  
  #correctPassword = 'tt'
  
  
  #observeEvent(input$loginButton, {
    # Check if the entered password is correct
   # if (input$password == correctPassword) {
    #  output$result <- renderText("Login successful!")
      
      # Show the main app UI
   #   shinyjs::enable("mainApp")
    #  shinyjs::show("mainApp")
    #} else {
     # output$result <- renderText("Incorrect password. Please try again.")
   # }
 # })
  
 #observeEvent( 'input.tabs == "Products"',updateTabsetPanel('tabss',session, selected = input.tabss))
}