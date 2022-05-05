# This script determine the number of games a player has set a records for
# This is a measure of their degree of generalism/specalisation. 

# requires
require(dplyr)
# source functions
source('code/analyse_data/function_for_player_analysis.R')

# load data
df <- read.csv('data/processed/speedrun_data_clean_concavity.csv', row.names = 1)

# count the number of unique records each player has
player_summary <- df %>% group_by(run_player_id) %>% 
  summarise(
    number_of_records_in_unique_games = length(unique(id)),
    number_of_records = length(id)
  )

# calculate time differentials per record
df <- df %>% group_by(id) %>%
  group_modify(~calculate_time_difference_between_records(.))

# calculate record longevity per record
df <- df %>% group_by(id) %>%
  group_modify(~calculate_longevity_of_record(.))

# calculate record longevity per record in percentage
df <- df %>% group_by(id) %>%
  group_modify(~calculate_longevity_of_record_percentage(.))

# merge data with player data
df <- df %>% left_join(player_summary)

# count the number of unique players in each record
game_summary <- df %>% group_by(id) %>% 
  summarise(
    number_of_unique_players_in_game = length(unique(run_player_id)),
    number_of_players_in_game = length((run_player_id))
  )

# merge data with game data
df <- df %>% left_join(game_summary)

# write data to file
write.csv(df, 'data/processed/speedrun_data_clean_concavity_player_game.csv')

df %>% filter(id == 'yd478gde5dw43j0kewpn8nj9') %>% ggplot(data = ., aes(x = run_date_in_days, y = run_time_percentage)) + geom_point()

