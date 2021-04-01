item_boxplots <- function(data, group_var, items){
  data.m <- data %>% select(group_var, items) %>% melt()
  
  p <- ggplot(data.m, aes(x= group_var, y = value)) + geom_boxplot() + facet_wrap(~variable, ncol = 4)
  print(p)
}