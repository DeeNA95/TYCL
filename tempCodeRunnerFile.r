plots <- lapply(unique(tot_test2$ProductGroup), function(group) {
  subset_df <- df[tot_test2$ProductGroup == group, ]
  
  plot_ly(
    data = subset_df,
    x = ~month,
    y = ~Total,
    color = ~year,
    type = "bar",
    showlegend = FALSE
  ) %>%
    layout(
      title = paste("Product Group:", group),
      xaxis = list(title = "Month"),
      yaxis = list(title = "Total"),
      barmode = "stack"
    )
})

# Arrange the plots using subplot
facet_plot <- subplot(
  plots,
  nrows = length(plots),
  margin = 0.05
)

# Show the facet wrap plot
facet_plot
