ggplot(tot_test2, aes(month, Total, fill = year)) +
  geom_col() +
  facet_wrap(~ProductGroup, scales = "free_y") +
  theme_minimal()
