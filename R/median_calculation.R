median_calculation <- function(df, price_list, aggregation){
  
  y <- df %>% select(aggregation, price_list) %>% group_by(!!sym(aggregation)) %>% summarise_all(funs(median(., na.rm = TRUE)))
  print(y)
  
}




