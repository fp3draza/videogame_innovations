# process api data

rm(list=ls())

load('api_data_2022-04-13.rdata')

df <- do.call("rbind", leaderboard_list)


require(ggplot2)
require(stringr)
require(lubridate)
require(dplyr)

# make unique id for each record
df <- df %>% mutate(id = paste0(game_id_string, category_id_string, level_id_string))

# keep only records that have at least five observations
df <- df %>% group_by(id) %>% filter(n() > 5)


# check to see if data is in correct order
# if data is incorrect, set to na
df <- df %>% 
  group_by(id) %>% 
  group_modify(~check_data_for_correct_order(.)) %>% 
  na.omit()

# remove any data where 0 run time is recorded
ids_with_incorrect_times <- df %>% filter(run_time == 0) %>% select(id)
ids_with_incorrect_times <- ids_with_incorrect_times$id
df <- df %>% filter(!id %in% ids_with_incorrect_times)

# convert run date to time (in seconds) since first record 
# note that first record has its time set to second = 1
df <- df %>% group_by(id) %>%
  mutate(run_date_in_seconds = as.vector(run_date - min(run_date)) + 1)

# convert run time so that first time is set to 1 
# and rest are difference from first time
df_test <- df %>% group_by(id) %>%
  mutate(run_time_standard = as.vector(run_time - max(run_time)))


aaa <- df %>% filter(id == unique(df$id)[1]) %>%
  select(run_date_submitted)

aaa$run_date_submitted

ggplot(data = df_test %>% filter(id == unique(df$id)[123]), aes(x = run_date_in_seconds, y = run_time_standard)) + geom_point() 
 
