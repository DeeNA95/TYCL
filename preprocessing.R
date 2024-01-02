source('packages.R')


#SOON TO BE DEPRECATED
##product list
products = read_csv('products.csv')
products = distinct(products)
product_groups = unique(products$ProductGroup)
products_headers = names(products)
products_headers[3] = "Code"
names(products) = products_headers


#load and clean up function
load_month = function(month, year, directory = 'data') {
  # month should be in quotes and year should be 2 digits
  selection = c('Code', 'Product', 'Quantity', 'Total')

  read_xlsx(paste0(directory,'/', month, ' ', year, '.xlsx'), trim_ws = T) %>%
    select(all_of(selection)) %>%
    subset(!(Code %in% c('NA', 'Code')), Total != 'NA') %>%
    filter(Total != 'NA', Quantity != 'Quantity') %>%

    mutate(
      Code = as.character(Code),
      Total = as.numeric(Total),
      Quantity = as.numeric(Quantity),
      Product = as.character(Product)
    )
}

# Loading months
active_years = c(22, 23)
all_data_frames = list()

## explore replacing with lapply
  for (y in active_years) {
    for (i in month.abb) {
      file_path <- paste0('data/', i, ' ', y, '.xlsx')

      if (file.exists(file_path)) {
        processed_name <- paste0(i, y)
        data_frame <- load_month(i, y) %>% mutate(month = i, year = y)

        all_data_frames[[processed_name]] <- data_frame
      }
  }
}

# Combining data frames
tot_test <- bind_rows(all_data_frames)

# Factorize month and year columns
tot_test$month = factor(tot_test$month, levels = month.abb)
tot_test$year = factor(tot_test$year, levels = c(22, 23))


pgjoin = data.frame(products[, c(1:3)])


tot_test2 = left_join(tot_test, pgjoin, by = "Code")
tot_test2 = tot_test2 %>% select(-Name) %>% arrange(desc(Total))

#beef tripe amalgamation

tot_test2$Product = str_replace_all(
  tot_test2$Product,
  'Beef Tripe \\(Intestines\\)  Yemad3E',
  'Beef Tripe \\(Intestines\\) Yemad3E'
)

product_type = c('Chicken','Layer','Back',
                 'Drumstick','Kpanla', 'Tripe','Salmon',
                 'Lele','Olonka','Frytol','Mom','Oba')


top_products_meat = head(tot_test2 %>%
                           group_by(Product) %>%
                           filter(ProductGroup %in% 'Meat\\Fish') %>%
                           summarise(ProductValue = sum(Total))%>% 
                           arrange(desc(ProductValue)) %>% 
                           select(1),
                         7)
top_products_rice = head(tot_test2 %>%
                           group_by(Product) %>%
                           filter(ProductGroup %in% 'Rice') %>%
                           summarise(ProductValue = sum(Total))%>%
                           arrange(desc(ProductValue)) %>%
                           select(1),
                         5)
top_products_oil = head(tot_test2 %>%
                          group_by(Product) %>%
                          filter(ProductGroup %in% 'Oil') %>%
                          summarise(ProductValue = sum(Total)) %>%
                          arrange(desc(ProductValue)) %>%
                          select(1),
                      3)
top_products_sugar = head(tot_test2 %>%
                            group_by(Product) %>%
                            filter(ProductGroup %in% 'Sugar') %>%
                            summarise(ProductValue = sum(Total))%>%
                            arrange(desc(ProductValue)) %>% 
                            select(1),
                          2)
top_products_tomatoe = head(tot_test2 %>%
                              group_by(Product) %>%
                              filter(ProductGroup %in% 'Tomato Paste') %>%
                              summarise(ProductValue = sum(Total)) %>% 
                              arrange(desc(ProductValue)) %>%
                              select(1),
                            3)

minor_pgroups = c("Sardine", "Ketchup\\Mayonnaise\\Baked Beans",
                  "Indomie", "Beverages", "Tuna Flakes\\Chunks",
                  "Mackerel", "Spaghetti", "Spices")

external_pgroups = c("External", "Mabel Spices")


q1 = tot_test2 %>% filter(month %in% c('Jan','Feb','Mar'))
q2 = tot_test2 %>% filter(month %in% c('Apr','May','Jun'))
q3 = tot_test2 %>% filter(month %in% c('Jul','Aug','Sep'))

q4 = tot_test2 %>% filter(month %in% c('Oct','Nov'))

for(m in month.abb){
  mname = paste(m)
  mdata = tot_test2 %>% filter(month == m)
  assign(mname, mdata)
}

 Jan24 = Jan %>% mutate(ExTotal = round(Total * 2.45,1),ExQuantity = round(Quantity * 2.35 ,1), year = factor(24)) %>% select(-Total,-Quantity)
 Feb24 = Feb %>% mutate(ExTotal = round(Total * 2.05,1),ExQuantity = round(Quantity * 1.95 ,1), year = factor(24)) %>% select(-Total,-Quantity)
 Mar24 = Mar %>% mutate(ExTotal = round(Total * 1.6,1),ExQuantity = round(Quantity * 1.5 ,1), year = factor(24)) %>% select(-Total,-Quantity)

 q224 = q2 %>% mutate(ExTotal = round(Total * 1.15,1),ExQuantity = round(Quantity * 1.1 ,1), year = factor(24)) %>% select(-Total,-Quantity) 

 q324 = q3 %>% filter(year == 23) %>% mutate(ExTotal = round(Total * 1.14,1),ExQuantity = round(Quantity * 1.07 ,1), year = factor(24)) %>% select(-Total,-Quantity)

 q424 = q4 %>% filter(year == 23) %>% mutate(ExTotal = round(Total * 1.14,1),ExQuantity = round(Quantity * 1.07 ,1), year = factor(24)) %>% select(-Total,-Quantity) 

 Dec24 = Dec %>% filter(year == 23)%>% mutate(ExTotal = round(Total * 1.2,1),ExQuantity = round(Quantity * 1.2 ,1), year = factor(24)) %>% select(-Total,-Quantity) 

 predict24 = bind_rows(Jan24, Feb24, Mar24, q224, q324, q424, Dec24)
