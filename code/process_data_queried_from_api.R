# require
require(dplyr)
require(ggplot2)
require(stringr)
require(lubridate)

# define function to nudge records that were established on the same day
nudge_records_from_same_date <- function(dataframe_to_use){
  
  # check to see if there are duplicated elements
  while (any(duplicated(dataframe_to_use$normalised_run_date))) {
    
    # find index of duplicated dates
    index_of_duplicates <- which(duplicated(dataframe_to_use$normalised_run_date)) - 1
    
    # nudge them to the past
    dataframe_to_use$normalised_run_date[index_of_duplicates] <- dataframe_to_use$normalised_run_date[index_of_duplicates] + 0.000001
    
  }
  
  # return
  return(dataframe_to_use)
  
}

# define function to make sure that data is ordered by run time, if not discard
check_data_for_correct_order <- function(dataframe_to_use){
  
  if (is.unsorted(dataframe_to_use$run_time) || is.unsorted(dataframe_to_use$run_date[1] - dataframe_to_use$run_date)) {
    
    dataframe_to_use$run_time <- NA
    
  }
  
  return(dataframe_to_use)
  
}

# define function to make sure that data is ordered by run time, if not discard
check_data_for_min_0_and_max_1 <- function(dataframe_to_use){
  
  if (!dataframe_to_use$normalised_run_date[1] == 1 || !dataframe_to_use$normalised_run_date[length(dataframe_to_use$normalised_run_date)] == 0) {
    dataframe_to_use$run_time <- NA
  }
 return(dataframe_to_use)
}

# define function to calculate how much world record times have reduced
calculate_world_record_time_reduction <- function(dataframe_to_use){
  
  dataframe_to_use$delta_normalised_run_time <-c(0, dataframe_to_use$normalised_run_time[2:length(dataframe_to_use$normalised_run_time)] - dataframe_to_use$normalised_run_time[1:(length(dataframe_to_use$normalised_run_time) - 1)])
  
  return(dataframe_to_use)

}

# read data 
leaderboard_data <- read.csv('world_record_data.csv', row.names = 1)

# post process speedrun data
# first clean up submission date to include time
processed_leaderboard_data <- leaderboard_data  %>% 
  mutate(clean_date = ymd_hms(str_replace(substr(run_date_submitted,1,nchar(run_date_submitted)-1), 'T', ' ')))

# create new dummy variable to summarise game, category, and level
processed_leaderboard_data <- processed_leaderboard_data %>% 
  mutate(record_compound_id = paste0(game_id_string, category_id_string, level_id_string))

# remove records where there is a 0 time recorded
ids_with_incorrect_times <- processed_leaderboard_data %>% filter(run_time == 0) %>% select(record_compound_id)
ids_with_incorrect_times <- ids_with_incorrect_times$record_compound_id
processed_leaderboard_data <- processed_leaderboard_data %>% filter(!record_compound_id %in% ids_with_incorrect_times)

# next group by game, category and level and normalise run date and run time
processed_leaderboard_data <- processed_leaderboard_data %>% 
                                group_by(record_compound_id) %>% 
                                mutate(normalised_run_date = (as.numeric(as.Date(run_date) - min(as.Date(run_date)))) / as.numeric(max(as.Date(run_date)) - min(as.Date(run_date))),
                                normalised_run_time = (run_time - min(run_time)) / (max(run_time) - min(run_time)))

# reverse the time progression of runs where this was flipped (problem from original data source)
ids_to_fix <- processed_leaderboard_data %>% filter(normalised_run_date == 1, normalised_run_time == 1) %>% select(record_compound_id)
ids_to_fix <- ids_to_fix$record_compound_id
processed_leaderboard_data_subset_fixed <- processed_leaderboard_data %>% 
                                filter(record_compound_id %in% ids_to_fix) %>% 
                                group_by(record_compound_id) %>%
                                mutate(normalised_run_time = 1 - normalised_run_time) 

# remove the observation that needed fixing
processed_leaderboard_data <- processed_leaderboard_data %>% 
                                filter(!record_compound_id %in% ids_to_fix)

# join large dataset with fixed ids
processed_leaderboard_data <- rbind(processed_leaderboard_data,processed_leaderboard_data_subset_fixed)

# get rid of any missing values
processed_leaderboard_data <- processed_leaderboard_data %>% 
                                na.omit()

# fix cases in which record was established on the same date
processed_leaderboard_data <- processed_leaderboard_data %>% 
                                group_by(record_compound_id) %>% 
                                group_modify(~nudge_records_from_same_date(.))

# remove cases where runtime were not in order
processed_leaderboard_data <- processed_leaderboard_data %>% 
                                group_by(record_compound_id) %>% 
                                group_modify(~check_data_for_correct_order_in_run_time(.)) %>% 
                                na.omit()

# calculate difference in world record time
processed_leaderboard_data <- processed_leaderboard_data %>% 
                                group_by(record_compound_id) %>% 
                                group_modify(~calculate_world_record_time_reduction(.)) %>% 
                                filter(delta_normalised_run_time >= 0)

# remove any other strange data
processed_leaderboard_data <- processed_leaderboard_data %>% filter(delta_normalised_run_time != 1)
processed_leaderboard_data <- processed_leaderboard_data %>% group_by(record_compound_id) %>% filter(n() > 5)
processed_leaderboard_data <- processed_leaderboard_data %>% group_by(record_compound_id) %>% group_modify(~check_data_for_min_0_and_max_1(.))
processed_leaderboard_data <- processed_leaderboard_data %>% na.omit()

# save data
write.csv(processed_leaderboard_data, file = 'clean_world_record_data.csv')

