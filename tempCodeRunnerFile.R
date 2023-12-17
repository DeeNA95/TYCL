ot_test2 %>% subset(Product %in% input$pin) %>%
      group_by(year, month) %>%
      summarise(Total = sum(round(Total, 0))) %>%
      mutate(MTD = cumsum(Total), MTD = comma_format()(MTD)) %>%
      select(year,month,MTD) %>% 
      pivot_wider(names_from = month,
                  values_from = c( "MTD"),
                  names_sep = "_")  %>% 
      select(9)