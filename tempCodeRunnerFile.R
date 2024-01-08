sales %>%
        summarise(
          Total = round(sum(Total), 2),
          Quantity = round(sum(Quantity), 2),
          .by = c(Name, Product_Group, Date, Month, Year, Day, Product_Group)
        ) %>%
        arrange(desc(Total))