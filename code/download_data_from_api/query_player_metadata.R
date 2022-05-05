# requires
require(dplyr)

fetch_player_metadata_from_api <- function(player_id_string){
  
  # build command to query list of categories
  player_string <- paste0('https://www.speedrun.com/api/v1/users/', player_id_string)
  
  # try to fetch data from api
  player_data <- try(jsonlite::fromJSON(player_string), silent = TRUE)
  player_runs <- try(jsonlite::fromJSON(paste0('https://www.speedrun.com/api/v1/runs?user=', player_id_string)))
  player_pb <- try(jsonlite::fromJSON(paste0('https://www.speedrun.com/api/v1/users/', player_id_string,'/personal-bests')))
  
  # if the request gives an erorr, return empty data
  if (class(player_data) == 'try-error' ) {
    
    country_code <- c("NA")
    
    has_twitch <- c("NA")
    
    has_youtube <- c("NA")
    
    date_signup <- c("NA")
    
    developers <- c("NA")
    
    publishers <- c("NA")
    
    discord <- c("NA")
    
    require_video <- c("NA")
    
    emulators_allowed <- c("NA")
    
    level_data <- c(game_id_string, released, romhack, platforms, genres, developers, publishers,
                    discord, require_video, emulators_allowed)
    
  }
  
  else{
    
    # keep only data
    country_code <- player_data$data$location$country$code
    
    has_twitch <- TRUE
    
    if (is.null(player_data$data$twitch$uri)) {
      has_twitch <- FALSE
    }
    
    has_youtube <- TRUE
    
    if (is.null(player_data$data$twitch$youtube$uri)) {
      has_youtube <- FALSE
    }
    
    date_signup <- as.Date(player_data$data$signup)
    
   
    level_data <- c(game_id_string, released, romhack, platforms, genres, developers, publishers,
                    discord, require_video, emulators_allowed)
    
  }
  
  # return data
  return(level_data)
}

get_game_metadata <- function(data_to_use){
  
  # obtain list of game ids
  list_of_game_ids <- unique(data_to_use$game_id_string)
  
  # create empty dataframe
  df <- NULL
  
  for (game in list_of_game_ids) {
    
    print(game)
    
    # fetch metadata for current game
    current_game_metadata <- fetch_game_metadata_from_api(game)
    
    # add metadata to dataframe
    df <- rbind(df, current_game_metadata)
    
  }
  
  df <- as.data.frame(df)
  rownames(df) <- NULL
  colnames(df) <- c('game_id_string', 'year_released', 'romhack', 'number_platforms','genre','developer',
                    'publisher', 'discord', 'require_video', 'emulators_allowed')
  
  # return
  return(df)
}

