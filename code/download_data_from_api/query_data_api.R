# requires
require(jsonlite)
require(dplyr)
require(RSQLite)
require(dbplyr)

fetch_game_information_from_api <- function(){
  
  # initialize size of data dump
  running <- TRUE
  
  # define the iteration step
  iteration_step  <- 200
  
  # define initial iteration
  iteration <- 13500
  counter <- 0
  
  # initialize empty list
  leaderboard_list <- list()

  # while loop
  while (running <- TRUE) {
    
    # build command to query list of all games
    list_of_games_query_string <- paste0('https://www.speedrun.com/api/v1/games?offset=',iteration,'&max=200')
    
    # fetch data from api
    queried_data <- jsonlite::fromJSON(list_of_games_query_string)
    
    # determine the size of data dump
    pag_size <- queried_data$pagination$size
    
    # check if data query is empty
    if (pag_size == 0) {
      
      # if so end loop
      running <- FALSE
      break
    }
    
    # if query not empty
    else{
      
      # keep only data
      queried_data <- queried_data$data
      
      # keep only relevant columns (for now this is only keeping the game id)
      #queried_data <- queried_data %>% select(id, abbreviation, released) %>% mutate(names = queried_data$names$international)
      queried_data <- queried_data %>% select(id) 
      
      # tidy data
      queried_data <- queried_data %>% rename(game_id = id)
      
      # loop through each game id
      for(game_name in queried_data$game_id){
        
        print(paste0(counter,' : ',game_name))

        # try to build the leaderboard of the game 
        game_leaderboard <- try(build_leaderboard_dataframe_for_game(game_name), silent = TRUE)
        
        # if the request does not give an error
        if((class(game_leaderboard)!="try-error")) { 
          
          # if the leaderboard is not empty
          if (nrow(game_leaderboard) > 0) {
          
            # add the leaderboard to the list
            leaderboard_list[[length(leaderboard_list)+1]] <- build_leaderboard_dataframe_for_game(game_name)
          }
        }
    
        # update counter 
        counter <- counter + 1
      }
      

    }
    
    # update iteration
    iteration <- iteration + iteration_step
    # to avoid throttling, give api a rest of 60 seconds
    Sys.sleep(60)
    # save objects in case api breaks
    save(leaderboard_list, iteration, file = "api_data.RData")
    
  }

  # convert list of list to dataframe
  leaderboard_dataframe <- do.call(rbind, leaderboard_list)
  
  # Return
  return(leaderboard_dataframe)
}


fetch_category_information_from_api <- function(game_id_string){
  
  # build command to query list of categories
  list_of_categories_query_string <- paste0('https://www.speedrun.com/api/v1/games/', game_id_string, '/categories')
  
  # try to fetch data from api
  category_data <- try(jsonlite::fromJSON(list_of_categories_query_string), silent = TRUE)
  
  # if the request gives an erorr, return empty data
  if (class(category_data) == 'try-error' ) {
    
    category_id <- c("NA")
    
    name <- c("NA")
    
    type <- c("NA")
    
    level_data <- data.frame(category_id, name, type)
    
  }
  
  else{
    
    # keep only data
    category_data <- category_data$data
  
    # keep only relevant data
    category_data <- category_data %>% select(id, name, type)
  
    # tidy data
    category_data <- category_data %>% rename(category_id = id) %>% mutate(game_id = game_id_string)
  }
  
  # return data
  return(category_data)
}

fetch_level_information_from_api <- function(game_id_string){
  
  # build command to query level from given game
  list_of_level_query_string <- paste0('https://www.speedrun.com/api/v1/games/', game_id_string, '/levels')
  
  # try to fetch data from api
  level_data <- try(jsonlite::fromJSON(list_of_level_query_string), silent = TRUE)
  
  # if the request gives an erorr, return empty data
  if (class(level_data) == 'try-error' ) {
    
    id <- c("NA")
    name <- c("NA")
    level_data <- data.frame(id, name)
    
  }
  
  else if (length(level_data$data) == 0) {
    
    id <- c("NA")
    name <- c("NA")
    level_data <- data.frame(id, name)
    
  }
  
  else{
    
    # select relevant data
    level_data <- level_data$data
    level_data <- level_data %>% select(id, name)
  }
  
  # return statement
  return(level_data)
}

fetch_run_information_from_api_per_game <- function(game_id_string, category_id_string){
  
  # empty level id
  level_id_string <- 'game-level'
  
  # build command to query list of run for given category of given game
  list_of_runs_query_string <- paste0('https://www.speedrun.com/api/v1/leaderboards/', game_id_string,'/category/', category_id_string)

  # try to fetch data from api
  leaderboard_data <- try(jsonlite::fromJSON(list_of_runs_query_string), silent = TRUE)
  
  # if the request gives an erorr, return empty data
  if (class(leaderboard_data) == 'try-error') {
    run_time <- NA
    run_date <- NA
    run_date_submitted <- NA
    run_player_id <- NA
  }
  
  else if (length(leaderboard_data$data) == 0) {
    
    id <- c("NA")
    name <- c("NA")
    level_data <- data.frame(id, name)
    
  }
  
  else{
    
    # keep relevant data
    leaderboard_data <- leaderboard_data$data$runs
  
    # if no data is recorded for the level, return a NULL
    if (is.null(nrow(leaderboard_data))) {
      run_time <- NA
      run_date <- NA
      run_date_submitted <- NA
      run_player_id <- NA
    }
  
    if (!is.null(nrow(leaderboard_data))) {
      # select relevant columns 
      run_time <- as.numeric(leaderboard_data$run$times$realtime_t)
      run_date <- leaderboard_data$run$date
      run_date_submitted <- leaderboard_data$run$submitted
      run_player_id <- unlist(sapply(leaderboard_data$run$players,function(x) x[2]))
    }
  }

  # merge columns into dataframe 
  leaderboard_df <- as_tibble(cbind(game_id_string, category_id_string, level_id_string, run_time, run_date, run_date_submitted, run_player_id), row.names = c(''))
  
  # return statement
  return(leaderboard_df)
  
  
}


fetch_run_information_from_api_per_level <- function(game_id_string, category_id_string, level_id_string){
  
  # build command to query list of run for given category of given game
  list_of_runs_query_string <- paste0('https://www.speedrun.com/api/v1/leaderboards/', game_id_string,'/level/',level_id_string,'/', category_id_string)
  
  # try to fetch data from api
  leaderboard_data <- try(jsonlite::fromJSON(list_of_runs_query_string), silent = TRUE)
  
  # init variables 
  run_time <- NA
  run_date <- NA
  run_date_submitted <- NA
  run_player_id <- NA
  
  # if the request gives an erorr, return empty data
  if (class(leaderboard_data) == 'try-error' ) {
    run_time <- NA
    run_date <- NA
    run_date_submitted <- NA
    run_player_id <- NA
  }
  
  else{
    # keep relevant data
    leaderboard_data <- leaderboard_data$data$runs
  
    # if no data is recorded for the level, return a NULL
    if (is.null(nrow(leaderboard_data))) {
      run_time <- NA
      run_date <- NA
      run_date_submitted <- NA
      run_player_id <- NA
    }
  
    if (!is.null(nrow(leaderboard_data)) && !any(sapply(leaderboard_data$run$players, function(x) nrow(x)) == 0)) {
      # select relevant columns 
      run_time <- as.numeric(leaderboard_data$run$times$realtime_t)
      run_date <- leaderboard_data$run$date
      run_date_submitted <- leaderboard_data$run$submitted
      run_player_id <- unlist(sapply(leaderboard_data$run$players,function(x) x[2]))
    }
  }
    # merge columns into dataframe 
    leaderboard_df <- as_tibble(cbind(game_id_string, category_id_string, level_id_string, run_time, run_date, run_date_submitted, run_player_id), row.names = c(''))
  
  # return statement
  return(leaderboard_df)
  
  
}

build_leaderboard_dataframe_for_game <- function(game_id_string){
  
  # create empty dataframe
  dataframe_to_fill <- NULL
    
  # fetch all categories for game
  categories_for_game <- fetch_category_information_from_api(game_id_string = game_id_string)
  
  # check if the current game has per-level leader boards
  if (any(categories_for_game$type == 'per-level')) {
    
    # find all levels
    levels_for_game <- fetch_level_information_from_api(game_id_string = game_id_string)
    
    # find all the category ids for per-level leaderboards
    category_ids_for_per_level_lbs <- categories_for_game[which(categories_for_game$type == 'per-level'),1]
    
    # get all possible combination of category and level
    category_level_combo <- expand.grid(category_ids_for_per_level_lbs, levels_for_game$id)
    
    # loop through category ids for each level
    for (i in 1:nrow(category_level_combo)) {
        # extract the current combination of category and level
        current_category <- as.character(category_level_combo[i,1])
        current_level <- as.character(category_level_combo[i,2])
        print(current_category)
        print(current_level)
        # fetch leaderboard 
        current_leaderboard <- fetch_run_information_from_api_per_level(game_id_string, category_id_string = current_category, level_id_string = current_level)
        
        # bind to main dataframe 
        dataframe_to_fill <- rbind(dataframe_to_fill, current_leaderboard)
    }
  }
  
  # categories per game at the game level
  categories_for_game_at_game_level <- categories_for_game[which(categories_for_game$type != 'per-level'),1]
  
  # loop through categories ids at game level
  for (i in categories_for_game_at_game_level) {
    
    # fetch leaderboard
    current_leaderboard_gamelevel <- fetch_run_information_from_api_per_game(game_id_string = game_id_string, category_id_string = i)
    # bind to main dataframe 
    dataframe_to_fill <- rbind(dataframe_to_fill, current_leaderboard_gamelevel)
    
  }
 
  # clean data and build world record history
  dataframe_to_fill <- process_leaderboard_data(dataframe_to_fill)

  # return complete leaderboard for game
  return(dataframe_to_fill)
  
}


process_leaderboard_data <- function(leaderboard_dataframe){
  
  # convert data into numeric and dates
  leaderboard_dataframe <- leaderboard_dataframe %>% na.omit() %>% mutate(run_time = as.numeric(run_time), run_date = as.Date(run_date))
  
  # group by game, category and level id and build world record history for each group
  leaderboard_dataframe <- leaderboard_dataframe %>% group_by(game_id_string, category_id_string, level_id_string) %>% group_modify(~build_world_record_history(.))
  
  # keep only record with three or more times
  leaderboard_dataframe <- leaderboard_dataframe %>% group_by(game_id_string, category_id_string, level_id_string) %>% filter(n() >= 5)
  
  # return statement
  return(leaderboard_dataframe)
  
}

build_world_record_history <- function(leaderboard){
  
  # initialize iterator
  iterator <- 1
  
  # while there are still rows to order
  while (iterator != nrow(leaderboard) && iterator < nrow(leaderboard)) {
    
    # set the current year to test to the one specified by iterator
    current_year_test <- leaderboard$run_date[iterator]
    
    # determine which entries to remove, based on whether the time was recorded after the previous fastest time
    entries_to_remove <- which(current_year_test < leaderboard$run_date[iterator:length(leaderboard$run_date)])
    
    # if there are rows to remove
    if (length(entries_to_remove) != 0) {
      
      # fix the indexing
      entries_to_remove <- entries_to_remove + (iterator - 1)
      
      # remove the entries
      leaderboard <- leaderboard[-entries_to_remove, ] 
    }
    
    # update the iterator
    iterator <- iterator + 1 
  }
  
  # return
  return(leaderboard)
}


