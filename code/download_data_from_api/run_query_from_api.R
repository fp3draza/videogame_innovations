# source functions
source('query_data_api.R')

# run function
speed_run_database <- fetch_game_information_from_api()

# write 
# write.csv(speed_run_database, '../data/XX')
