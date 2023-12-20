dfs = list()
plots= list()
for(group in unique(tot_test2$ProductGroup)){
    name <- group
    data_frame <- tot_test2 %>% 
    subset(tot_test2$ProductGroup == group)%>% group_by(month,year) %>% summarise(Total = sum(Total))
    dfs[[name]] <- data_frame
}

for (name in names(dfs)){
    df = dfs[[name]]
   p <- plot_ly(
        data = df,
        x = ~month,
        y = ~Total,
        color = ~year,
        type = 'bar'
    ) %>% 
    layout(title = name)
    plots[[name]] = p
}

grid.arrange(grobs = plots, ncol =3)

plots 
