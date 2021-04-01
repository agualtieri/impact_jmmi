minium_standards <- function(data, ...){
  
  data %>%  group_by(...) %>%
    summarise(., n = n()) %>% mutate(minimum_standards = ifelse(n >= 3, "requirement met", "requirement not met"))
  
}

