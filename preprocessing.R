source('packages.R')

##product list
products = read_csv('products.csv')
products = distinct(products)
product_groups = unique(products$ProductGroup)
products_headers = names(products)
products_headers[3] = "Code"
names(products) = products_headers


#load and clean up function
load_month = function(month, year, directory = 'test') {
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

  for (y in active_years) {
    for (i in month.abb) {
      file_path <- paste0('test/', i, ' ', y, '.xlsx')

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

product_type = c('All','Chicken','Layer','Back',
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

minor_pgroups = c("Sardine","Ketchup\\Mayonnaise\\Baked Beans",
                  "Indomie" ,"Beverages","Tuna Flakes\\Chunks",
                  "Mackerel","Spaghetti","Spices")

external_pgroups = c("External","Mabel Spices")

