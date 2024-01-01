oct = tot_test2  %>%  filter(year == 23, month == 'Oct',!(ProductGroup  %in% external_pgroups))  %>% group_by(ProductGroup)  %>% summarise(Total = round(sum(Total),2))

nov = tot_test2  %>%  filter(year == 23, month == 'Nov',!(ProductGroup  %in% external_pgroups))  %>% group_by(ProductGroup)  %>% summarise(Total = round(sum(Total),2))

dec = tot_test2  %>%  filter(year == 23, month == 'Dec',!(ProductGroup  %in% external_pgroups))  %>% group_by(ProductGroup)  %>% summarise(Total = round(sum(Total),2))

