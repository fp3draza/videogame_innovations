# function to read in stored Rdata, convert it to dataframe and store
convert_rdata_to_dataframe <- function(){
  
  # file names 
  rdata_load_filename <-'../videogame_downloads/data/api_data_2022-04-14.RData'
  write_output_filename <-'../videogame_downloads/data/api_data_2022-04-14.csv'
  
  # load Rdata
  load(rdata_load_filename)

  # bind all dataframes in list
  df <- do.call("rbind", leaderboard_list)
  
  # write file out
  write.csv(df, write_output_filename)

}


# run function
convert_rdata_to_dataframe()


