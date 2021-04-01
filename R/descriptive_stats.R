descriptive_stats <- function(data, group_var, items){
  
 data %>% select(group_var, items) %>% 
                            group_by(!!sym(group_var)) %>% 
                                                 group_modify(~ {
                                                  .x %>%
                                                    purrr::map_dfc(fivenum) %>%
                                                    mutate(stats = c("min", "Q1", "median", "Q3", "max"))
                                                    })
 
}



  


  