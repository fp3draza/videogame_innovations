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

