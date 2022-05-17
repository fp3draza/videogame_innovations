# requires
require(ggplot2)
require(dplyr)
require(ggbiplot)
require(cowplot)

# read data
#fitting_parameters <- read.csv('data/processed/fit_res_2022-05-09.csv', row.names = 1)
fitting_parameters <- read.csv('../../data/processed/fit_res.csv', row.names = 1)
og_data <- read.csv('../../data/raw/speedrun_data_clean.csv', row.names = 1)
metadata_games <- read.csv('../../data/processed/game_metadata.csv', row.names = 1)
metadata_runs <- read.csv('../../data/processed/run_metadata.csv', row.names = 1)
metadata_players <- read.csv('../../data/data/processed/player_metadata.csv', row.names = 1)

# tidy data
og_data <- og_data %>% group_by(id) %>% dplyr::mutate(number_of_records = n())
#fitting_parameters <- fitting_parameters %>% filter(R_sq_cat >= 7)
fitting_parameters <- fitting_parameters %>% filter(tau0_err/tau0 < 0.4)
fitting_parameters <- fitting_parameters %>% 
  left_join(og_data) %>% 
  select(id, game_id_string, beta_cat, tau0_cat, R_sq_cat, beta, tau0, number_of_records) %>% 
  distinct()

metadata_games <- dplyr::rename(metadata_games, number_of_runs_game = number_of_runs)

# process player metadata to get measures per game
metadata_players <- metadata_players %>%
  group_by(id) %>% 
  dplyr::summarise(mean_number_of_run_per_player= mean(total_runs_per_game_per_player, na.rm = TRUE),
            mean_number_of_run_per_player_global = mean(total_runs_per_player, na.rm = TRUE),
            player_efficacy_global = mean(player_efficacy_global, na.rm = TRUE),
            player_efficacy_per_game = mean(player_efficacy_per_game, na.rm = TRUE))

# join data
pca_data <- fitting_parameters %>% 
  left_join(metadata_games)

pca_data <- pca_data %>% 
  left_join(metadata_runs)

pca_data <- pca_data %>% 
  left_join(metadata_players)

pca_data <- pca_data %>% group_by(id) %>% mutate(record_efficacy = number_of_records/number_of_runs)

pca_data <- pca_data %>% 
  select(year_released, median_comment_length, number_of_runs,days_in_database, 
         fraction_of_runs_played_on_emulated, number_unique_players, fraction_runs_by_unique_players,
         run_production_rate, beta_cat, tau0_cat, R_sq_cat, beta, tau0, mean_number_of_run_per_player_global, player_efficacy_global, record_efficacy, number_of_records) %>% na.omit()

pca_data <- pca_data[pca_data$number_of_runs < 500,]
#pca_data <- pca_data %>% filter(R_sq_cat == 9) 

# fit pca
#pca_fit <- prcomp(pca_data[,c('year_released', 'median_comment_length', 'number_of_runs','days_in_database', 
#                            'fraction_of_runs_played_on_emulated', 'number_unique_players', 'fraction_runs_by_unique_players',
#                             'run_production_rate', 'mean_number_of_run_per_player_global', 'player_efficacy_global', 'record_efficacy', 'number_of_records')],
#               center = TRUE, scale. = TRUE)

pca_fit <- prcomp(pca_data[,c('year_released', 'number_of_runs','days_in_database', 
                              'fraction_of_runs_played_on_emulated', 'number_unique_players',
                              'run_production_rate', 'record_efficacy', 'number_of_records')],
                  center = TRUE, scale. = TRUE)

# plot pca
a <- ggbiplot(pca_fit, groups = as.factor(pca_data$beta_cat), ellipse = TRUE, var.axes=TRUE, alpha = 0.3, varname.abbrev = FALSE) + 
  ggtitle('beta') + theme_minimal() + theme(aspect.ratio = 1, legend.title = element_blank()) + xlab('PC1\n(0.36% variance explained)') + ylab('PC2\n(0.27% variance explained)')
b <- ggbiplot(pca_fit, groups = as.factor(pca_data$tau0_cat), ellipse = TRUE, var.axes=FALSE, alpha = 0.3) + 
  ggtitle('tau0') + theme_minimal() + theme(aspect.ratio = 1, legend.title = element_blank())  + xlab('PC1\n(0.36% variance explained)') + ylab('PC2\n(0.27% variance explained)')
c <- ggbiplot(pca_fit, groups = as.factor(pca_data$R_sq_cat), ellipse = TRUE, var.axes=FALSE, alpha = 0.3) + 
  ggtitle('Rsq') + theme_minimal() + theme(aspect.ratio = 1, legend.title = element_blank())  + xlab('PC1\n(0.36% variance explained)') + ylab('PC2\n(0.27% variance explained)')

plot_grid(a,b, nrow = TRUE)

d <- ggplot(data = pca_data, aes(x = as.factor(beta_cat), y = record_efficacy)) + 
  geom_boxplot() + geom_jitter() + theme_minimal() + theme(aspect.ratio = 1) +
  xlab('\nbeta') + ylab('fraction of runs that\n resulted in a world record\n')

e <- ggplot(data = pca_data, aes(x = as.factor(tau0_cat), y = year_released)) + 
  geom_boxplot() + geom_jitter() + theme_minimal() + theme(aspect.ratio = 1) +
  xlab('\ntau0') + ylab('year game was released\n')

plot_grid(d,e)
