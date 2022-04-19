# process api data
# requires
require(ggplot2)
require(stringr)
require(lubridate)
require(dplyr)
source('code/process_data_queried_from_api.R')

# load data
df <- read.csv('speedrun_data.csv', row.names = 1)

# make unique id for each record
df <- df %>% mutate(id = paste0(game_id_string, category_id_string, level_id_string))

# keep only records that have at least five observations
df <- df %>% group_by(id) %>% filter(n() > 5)

# transform run date to Data format
df <- df %>% mutate(run_date = as.Date(run_date))

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
df <- df %>% group_by(id) %>%
  mutate(run_time_standard = as.vector(run_time - max(run_time)))

df <- df %>% group_by(id) %>% 
  mutate(run_time_percentage = as.vector((run_time * 100)/max(run_time)))

df <- df %>% group_by(id) %>% 
  mutate(normalised_run_date = (as.numeric(run_date - min(run_date))) / as.numeric(max(run_date) - min((run_date))),
         normalised_run_time = (run_time - min(run_time)) / (max(run_time) - min(run_time)))

# write file out
write.csv(df, 'speedrun_data_clean.csv')
