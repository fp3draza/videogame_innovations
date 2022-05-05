calculate_time_difference_between_records <- function(data_to_mod){
  
  # build vector with time differentials
  time_diff <- c(0, data_to_mod$run_time_percentage[2:length(data_to_mod$run_time_percentage)] - data_to_mod$run_time_percentage[1:(length(data_to_mod$run_time_percentage) - 1)])
  
  # add vector to dataframe
  data_to_mod$time_differential <- time_diff
  
  # return
  return(data_to_mod)
}

calculate_longevity_of_record <- function(data_to_mod){
  
  # build vector with record longevity
  record_longevity <- c(NA, abs(data_to_mod$run_date_in_days[2:length(data_to_mod$run_date_in_days)] - data_to_mod$run_date_in_days[1:(length(data_to_mod$run_date_in_days) - 1)]))
  
  # add vector to dataframe
  data_to_mod$record_longevity <- record_longevity
  
  # return
  return(data_to_mod)
  
}

calculate_longevity_of_record_percentage <- function(data_to_mod){
  
  # build vector with record longevity in percentage
  record_longevity_percentage <- data_to_mod$record_longevity/max(data_to_mod$run_date_in_days)
  
  # add vector to dataframe
  data_to_mod$record_longevity_percentage <- record_longevity_percentage
  
  # return
  return(data_to_mod)
  
  
}