# requires
require(dplyr)

fetch_player_metadata_from_api <- function(player_id_string){
  
  # build command to query list of categories
  player_string <- paste0('https://www.speedrun.com/api/v1/users/', player_id_string)
  
  # try to fetch data from api
  player_data <- try(jsonlite::fromJSON(player_string), silent = TRUE)
  player_runs <- try(jsonlite::fromJSON(paste0('https://www.speedrun.com/api/v1/runs?user=', player_id_string,'&max=200')), silent = TRUE)
  Sys.sleep(1)
  player_pb <- try(jsonlite::fromJSON(paste0('https://www.speedrun.com/api/v1/users/', player_id_string,'/personal-bests')), silent = TRUE)
  
  # if the request gives an erorr, return empty data
  if (class(player_data) == 'try-error' ) {
    
    country_code <- c("NA")
    
    has_twitch <- c("NA")
    
    has_youtube <- c("NA")
    
    date_signup <- c("NA")
    
    total_runs <- c("NA")
    
    games_played <- c("NA")
    
    median_comment_length <- c("NA")
    
    fraction_of_wr_runs <- c("NA")
    
    median_pb_position <- c("NA")
    
    player_data <- c(player_id_string, country_code, has_twitch, has_youtube, date_signup, total_runs, games_played,
                    median_comment_length, fraction_of_wr_runs, median_pb_position)
    
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
    
    date_signup <- substr((player_data$data$signup), start = 1, stop = 4)
    
    total_runs <- length(unique(player_runs$data$id))
    
    games_played <- length(unique(player_runs$data$game))
    
    median_comment_length <- median(nchar(player_runs$data$comment), na.rm = TRUE)
    
    fraction_of_wr_runs <- sum(player_pb$data$place == 1)/length(unique(player_runs$data$id))
    
    median_pb_position <- median(player_pb$data$place)
    
   
    player_data <- c(player_id_string, country_code, has_twitch, has_youtube, date_signup, total_runs, games_played,
                    median_comment_length, fraction_of_wr_runs, median_pb_position)
    
  }
  
  # return data
  return(player_data)
}

get_player_metadata <- function(data_to_use){
  
  # obtain list of game ids
  list_of_player_ids <- unique(data_to_use$run_player_id)
  
  # create empty dataframe
  df <- NULL
  # counter
  counter <- 1
  
  for (player in list_of_player_ids) {
    
    # fetch metadata for current player
    print(player)
    current_game_metadata <- fetch_player_metadata_from_api(player)
    counter <- counter + 1
    if (counter%%20 == 0) {
      Sys.sleep(7)
    }
    print(paste0('progress: ', round(counter/length(list_of_player_ids), digits = 3)))
          
    # add metadata to dataframe
    df <- rbind(df, current_game_metadata)
    
  }
  
  df <- as.data.frame(df)
  rownames(df) <- NULL
  colnames(df) <- c('player_id_string', 'country_code', 'has_twitch', 'has_youtube', 'date_signup', 'total_runs', 'games_played',
                    'median_comment_length', 'fraction_of_wr_runs', 'median_pb_position')
  
  # return
  return(df)
}

