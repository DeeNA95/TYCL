Dec %>% filter(year == 23)%>% mutate(ExTotal = round(Total * 2.8,1),ExQuantity = round(Quantity * 2.8 ,1), year = factor(24)) %>% select(-Total,-Quantity) 
