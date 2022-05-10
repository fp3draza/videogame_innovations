# requires
require(ggplot2)
require(dplyr)
require(ggbiplot)
require(cowplot)

# read data
fitting_parameters <- read.csv('data/processed/fit_res_2022-05-09.csv', row.names = 1)
og_data <- read.csv('data/processed/speedrun_data_clean.csv', row.names = 1)
metadata_games <- read.csv('data/processed/game_metadata.csv', row.names = 1)
metada_runs <- read.csv('data/processed/run_metadata.csv', row.names = 1)

# tidy data
fitting_parameters <- fitting_parameters %>% filter(R_sq_cat >= 7)
fitting_parameters <- fitting_parameters %>% 
  left_join(og_data) %>% 
  select(id, game_id_string, beta_cat, tau0_cat, R_sq_cat, beta, tau0) %>% 
  distinct()

metadata_games <- dplyr::rename(metadata_games, number_of_runs_game = number_of_runs)

# join data
pca_data <- fitting_parameters %>% left_join(metadata_games)
pca_data <- pca_data %>% left_join(metada_runs)
pca_data <- pca_data %>% 
  select(year_released, median_comment_length, number_of_runs,days_in_database, 
         fraction_of_runs_played_on_emulated, number_unique_players, fraction_runs_by_unique_players,
         run_production_rate, beta_cat, tau0_cat, R_sq_cat, beta, tau0) %>% na.omit()
pca_data <- pca_data[pca_data$number_of_runs < 500,]

# fit pca
pca_fit <- prcomp(pca_data %>% 
                 select(year_released, median_comment_length, number_of_runs,days_in_database, 
                        fraction_of_runs_played_on_emulated, number_unique_players, fraction_runs_by_unique_players,
                        run_production_rate),
               center = TRUE, scale. = TRUE)

# plot pca
a <- ggbiplot(pca_fit, groups = as.factor(pca_data$beta_cat), ellipse = TRUE, var.axes=TRUE, alpha = 0.3) + ggtitle('beta') + theme_minimal() + theme(aspect.ratio = 1)
b <- ggbiplot(pca_fit, groups = as.factor(pca_data$tau0_cat), ellipse = TRUE, var.axes=TRUE, alpha = 0.3) + ggtitle('tau0') + theme_minimal() + theme(aspect.ratio = 1)
c <- ggbiplot(pca_fit, groups = as.factor(pca_data$R_sq_cat), ellipse = TRUE, var.axes=TRUE, alpha = 0.3) + ggtitle('Rsq') + theme_minimal() + theme(aspect.ratio = 1)

plot_grid(a,b,c, nrow = TRUE)

ggplot(data = pca_data, aes(x = 1/run_production_rate, y = tau0)) + geom_point() + facet_grid(~as.factor(beta_cat))

