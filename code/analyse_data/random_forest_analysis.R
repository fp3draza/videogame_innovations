# requires
require(ggplot2)
require(dplyr)
require(e1071)
require(cowplot)
require(randomForest)
require(ROCR)

# read data
#fitting_parameters <- read.csv('data/processed/fit_res_2022-05-09.csv', row.names = 1)
fitting_parameters <- read.csv('data/processed/fit_res_2022-05-11.csv', row.names = 1)
og_data <- read.csv('data/processed/speedrun_data_clean.csv', row.names = 1)
metadata_games <- read.csv('data/processed/game_metadata.csv', row.names = 1)
metadata_runs <- read.csv('data/processed/run_metadata.csv', row.names = 1)
metadata_players <- read.csv('data/processed/player_metadata.csv', row.names = 1)

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

# subset data for support vector analysis
support_vector_data_beta <- pca_data[,c('year_released', 'number_of_runs','days_in_database', 
                              'fraction_of_runs_played_on_emulated', 'number_unique_players',
                              'run_production_rate', 'record_efficacy', 'number_of_records','beta_cat')]

support_vector_data_beta$beta_cat <- factor(support_vector_data_beta$beta_cat)

support_vector_data_tau0 <- pca_data[,c('year_released', 'number_of_runs','days_in_database', 
                                        'fraction_of_runs_played_on_emulated', 'number_unique_players',
                                        'run_production_rate', 'record_efficacy', 'number_of_records','tau0_cat')]

support_vector_data_tau0$tau0_cat <- factor(support_vector_data_tau0$tau0_cat)

# define training and test data
n <- nrow(support_vector_data_beta)
number_training_data <- round(n*0.60)  
set.seed(343)
training_index <- sample(n, number_training_data)   
train_data_beta <- support_vector_data_beta[training_index,]   
test_data_beta <- support_vector_data_beta[-training_index,]   
train_data_tau0 <- support_vector_data_tau0[training_index,]
test_data_tau0 <- support_vector_data_tau0[-training_index,] 

# fit SVM beta
svm_beta <- svm(as.factor(beta_cat)~., data=train_data_beta, 
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)

prediction_svm_beta <- predict(svm_beta, test_data_beta)
confusion_matrix_svm_beta <- table(as.factor(test_data_beta$beta_cat), prediction_svm_beta)
accuracy_svm_beta <- sum(diag(confusion_matrix_svm_beta))/length(test_data_beta$beta_cat)

# fit random forest beta
mtry <- tuneRF(train_data_beta[-9],train_data_beta$beta_cat, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=FALSE, plot=FALSE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


rf_beta <- randomForest(
  beta_cat ~ .,
  data=train_data_beta,
  ntree = 10000,
  mtry = best.m, 
  importance = TRUE
)

prediction_rf_beta <- predict(rf_beta, test_data_beta[,-9])
confusion_matrix_rf_beta <- table(as.factor(test_data_beta$beta_cat), prediction_rf_beta)
accuracy_rf_beta <- sum(diag(confusion_matrix_rf_beta))/length(test_data_beta$beta_cat)
varImpPlot(rf_beta)

# fit SVM tau0
svm_tau0 <- svm(as.factor(tau0_cat)~., data=train_data_tau0, 
                method="C-classification", kernal="radial", 
                gamma=0.1, cost=10)

prediction_svm_tau0 <- predict(svm_tau0, test_data_tau0)
confusion_matrix_svm_tau0 <- table(as.factor(test_data_tau0$tau0_cat), prediction_svm_tau0)
accuracy_svm_tau0 <- sum(diag(confusion_matrix_svm_tau0))/length(test_data_tau0$tau0_cat)


# fit random forest tau0
mtry <- tuneRF(train_data_tau0[-9],train_data_tau0$tau0_cat, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=FALSE, plot=FALSE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


rf_tau0 <- randomForest(
  tau0_cat ~ .,
  data=train_data_tau0,
  ntree = 10000, mtry = best.m, importance=TRUE
)

prediction_rf_tau0 <- predict(rf_tau0, test_data_tau0[,-9])
confusion_matrix_rf_tau0 <- table(as.factor(test_data_tau0$tau0_cat), prediction_rf_tau0)
accuracy_rf_tau0 <- sum(diag(confusion_matrix_rf_tau0))/length(test_data_tau0$tau0_cat)
varImpPlot(rf_tau0)
