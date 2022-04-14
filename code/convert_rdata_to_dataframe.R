# function to read in stored Rdata, convert it to dataframe and store
convert_rdata_to_dataframe <- function(){
  
  # load Rdata
  load('api_data.Rdata')
  
  # bind all dataframes in list
  df <- do.call("rbind", leaderboard_list)
  
  # write file out
  write.csv(df, 'api_data.csv')
  
}

# run function
convert_rdata_to_dataframe()

