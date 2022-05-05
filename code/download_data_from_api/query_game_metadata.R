# require
require(dplyr)

fetch_game_metadata_from_api <- function(game_id_string){

  # build command to query list of categories
  game_string <- paste0('https://www.speedrun.com/api/v1/games/', game_id_string)
  
  # try to fetch data from api
  game_data <- try(jsonlite::fromJSON(game_string), silent = TRUE)
  
  # if the request gives an erorr, return empty data
  if (class(game_data) == 'try-error' ) {
    
    released <- c("NA")
    
    romhack <- c("NA")
    
    platforms <- c("NA")
    
    genres <- c("NA")
    
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
    game_data <- game_data$data
    
    released <- game_data$released
    
    romhack <- game_data$romhack
    
    platforms <- length(game_data$platforms)
    
    genres <- unlist(game_data$genres[1])
    
    if (is.null(genres)) {
      genres <- NA
    }
    
    developers <- unlist(game_data$developers[1])
    
    if (is.null(developers)) {
      developers <- NA
    }
    
    publishers <- unlist(game_data$publishers[1])
    
    if (is.null(publishers)) {
      publishers <- NA
    }
    
    discord <- length(game_data$discord) > 0
    
    require_video <- game_data$ruleset$`require-video`
    
    emulators_allowed <- game_data$ruleset$`emulators-allowed`
    
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

