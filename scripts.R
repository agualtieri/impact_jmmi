## R scripts
library(tidyverse)
library(cluster)

# devtools::install_github("zackarno/butteR")
library(butteR)

## Medians

median_items <- data %>%                                                                            # full dataset
  select(-uuid, -enum_id, -index) %>% group_by(governorate, district, market) %>%               # select only the items and aggregation columns
  summarise_all(funs(median(., na.rm = TRUE)))                                        # aggregate by median


median_items_dis <- median_items %>%                                                                            # full dataset
  select(-market) %>% group_by(governorate, district) %>%               # select only the items and aggregation columns
  summarise_all(funs(median(., na.rm = TRUE))) 



smeb <- median_items %>% group_by(district) %>%  # the group_by can be changed based on the aggregation level you desire
  mutate(
    smeb_item1 = item1 * 0.5,
    smeb_item2 = item2 * 0.5,
    smeb_item3 = item3 * 0.5,
    smeb_item4 = item4 * 0.5,
    smeb_item5 = item5 * 0.5,
    smeb_item6 = item6 * 0.5
  ) 

print(head(smeb, 3))

# If NAs are present the following code will input the median value of the district of govenorate of choice to fill the blank
smeb_no_NAs <- smeb %>% group_by(district) %>%  # the group_by can be changed based on what median value you want to input
  mutate(
    smeb_item1= ifelse(is.na(smeb_item1), median(smeb_item1, na.rm=T), smeb_item1),
    smeb_item2= ifelse(is.na(smeb_item2), median(smeb_item2, na.rm=T), smeb_item2),
    smeb_item3= ifelse(is.na(smeb_item3), median(smeb_item3, na.rm=T), smeb_item3),
    smeb_item4= ifelse(is.na(smeb_item4), median(smeb_item4, na.rm=T), smeb_item4),
    smeb_item5= ifelse(is.na(smeb_item5), median(smeb_item5, na.rm=T), smeb_item5),
    smeb_item6= ifelse(is.na(smeb_item6), median(smeb_item6, na.rm=T), smeb_item6)
  ) %>% select(governorate, district, starts_with("smeb"))

print(head(smeb_no_NAs, 3))







# Trend analysis
pct_change_by_groups_all_numerics <- function (df, group_var, time_id) {
  group_var <- enquo(group_var)
  time_id <- enquo(time_id)
  df %>% group_by(!!group_var) %>% arrange(!!group_var, !!time_id) %>% 
    summarise(across(is.numeric, pct_change)) %>% filter(!is.na(!!time_id)) %>% 
    select(-!!time_id)
}

# Load datasets
month1 <- read.csv("./inputs/test_dataset_m1.csv", stringsAsFactors = FALSE)
month2 <- read.csv("./inputs/test_dataset_m2.csv", stringsAsFactors = FALSE)

# rbind and change to date format
new_df <- rbind(month1, month2) 
new_df$month <- as.Date(new_df$month, "%d/%m/%Y")
class(new_df$month)
                
# test                
pct_change_current_to_last <- new_df %>%
                butteR::pct_change_by_groups_all_numerics(group_var = district, time_id = month)


new_df$month_date<- lubridate::month(new_df$month)

butteR::pct_change_by_groups_all_numerics(df = new_df,group_var = district, time_id = month_date)
