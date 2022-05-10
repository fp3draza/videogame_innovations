# define functions

fetch_run_metadata_level <- function(game_id_string, category_id_string, level_id_string){
  
  # try querying data
  queried_data <- try(jsonlite::fromJSON(paste0('https://www.speedrun.com/api/v1/leaderboards/',game_id_string,'/category/',category_id_string)))
  
  # extract revelant data
  extracted_data <- extract_data(queried_data)
  
  return(extracted_data)
  

}

fetch_run_metadata_game <- function(game_id_string, category_id_string, level_id_string){
  
  # try querying data
  queried_data <- try(jsonlite::fromJSON(paste0('https://www.speedrun.com/api/v1/leaderboards/',game_id_string,'/level/',level_id_string,'/', category_id_string)))
  
  # extract revelant data
  extracted_data <- extract_data(queried_data)
  
  return(extracted_data)
  
}


extract_data <- function(queried_data_to_extract){
  
  # comment length
  median_comment_length <- median(nchar(queried_data_to_extract$data$runs$run$comment), na.rm = TRUE)
  mean_comment_length <- mean(nchar(queried_data_to_extract$data$runs$run$comment), na.rm = TRUE)
  
  # number of runs
  number_of_runs <- nrow(queried_data_to_extract$data$runs$run)
  
  # days in database
  days_in_database <- as.numeric(difftime(max(queried_data_to_extract$data$runs$run$date), min(queried_data_to_extract$data$runs$run$date)))
  
  # number of platforms played on
  number_of_platforms_played_on  <- length(unique(queried_data_to_extract$data$runs$run$system$platform))
  
  # number of runs played on emulated system
  fraction_of_runs_played_on_emulated <-  sum(queried_data_to_extract$data$runs$run$system$emulated)/nrow(queried_data_to_extract$data$runs$run)
  
  # number of unique players
  number_unique_players <- length(unique(unlist(lapply(queried_data_to_extract$data$runs$run$players,function(x) x[1,2]))))
  
  # fraction of runs by unique players
  fraction_runs_by_unique_players <- number_unique_players/number_of_runs
  
  # run production rate
  run_production_rate <- number_of_runs/days_in_database
  
  # build vector
  return_vector <- c(median_comment_length, mean_comment_length, number_of_runs, days_in_database, number_of_platforms_played_on, fraction_of_runs_played_on_emulated, number_unique_players, fraction_runs_by_unique_players,run_production_rate)
  
  # return
  return(return_vector)
  
  
}

get_run_metadata <- function(data_to_use){
  
  # obtain list of game ids
  list_of_run_ids <- unique(data_to_use$id)
  game_counter <- 1
  
  # create empty dataframe
  df <- NULL
  
  for (run in list_of_run_ids) {
    
    print(run)
    
    # keep only current run
    current_run <- data_to_use %>% filter(id == run)
    
    # check if run is at game level
    if (current_run$level_id_string == 'game-level') {
      
      # fetch data
      current_run_metadata <- fetch_run_metadata_level(current_run$game_id_string, current_run$category_id_string, current_run$level_id_string)
      
    }
    
    else{
      
      # fetch data
      current_run_metadata <- fetch_run_metadata_game(current_run$game_id_string, current_run$category_id_string, current_run$level_id_string)
      
    }
    
    # add run id
    current_run_metadata <- c(run, current_run_metadata)
    
    # add metadata to dataframe
    df <- rbind(df, current_run_metadata)
    game_counter <- game_counter + 1
    
    #if (game_counter %% 30 == 0) {
    #   print('here')
    #   Sys.sleep(60)
    # }
    
  }
  
  df <- as.data.frame(df)
  rownames(df) <- NULL
  colnames(df) <- c('id','median_comment_length', 'mean_comment_length', 'number_of_runs', 'days_in_database','number_of_platforms_played_on','fraction_of_runs_played_on_emulated',
                    'number_unique_players', 'fraction_runs_by_unique_players', 'run_production_rate')
  # return
  return(df)
}

# fetch new game metadata
# read data
fitting_parameters <- read.csv('data/processed/fit_res_2022-05-09.csv', row.names = 1)
og_data <- read.csv('data/processed/speedrun_data_clean.csv', row.names = 1)

# tidy data
fitting_parameters <- fitting_parameters %>% filter(R_sq_cat >= 7)
fitting_parameters <- fitting_parameters %>% 
  left_join(og_data) %>% 
  select(id, game_id_string, beta_cat, tau0_cat, R_sq_cat, category_id_string, level_id_string) %>% 
  distinct()

# run function to query data
queried_run_data <- get_run_metadata(fitting_parameters)
