player_summary <- player_network_full_data %>% 
  group_by(run_player_id) %>%
  summarise(mean_delta = mean(delta_normalised_run_time), mean_degree = mean(as.numeric(degree)),
            max_delta = max(delta_normalised_run_time))

ggplot(data = player_summary, aes(x = max_delta)) + geom_histogram()
ggplot(data = player_summary, aes(x = mean_delta)) + geom_histogram()
ggplot(data = player_summary, aes(x = mean_degree)) + geom_histogram()


ggplot(data = player_summary, aes(x = mean_degree, y = max_delta)) + geom_point() 
ggplot(data = player_summary, aes(x = impact, y = mean_degree)) + geom_jitter() 


player_summary <- player_summary %>% filter(max_delta > 0) %>% group_by(run_player_id) %>%
  mutate(impact = if_else(max_delta > 0.90, 'high', if_else(max_delta < 0.10, 'low', 'average')))

head(network_clean_data)

# CODE FOR CONVEX/CONCAVE CURVE
head(data)

ggplot(data %>% filter(delta_normalised_run_time > 0), aes(x = normalised_run_date, y = delta_normalised_run_time)) + 
  geom_line(aes(group = record_compound_id), size = 0.02) + 
  theme_minimal() + xlab('Normalised run date') + ylab('Normalised run time') 



library(drc)
library(nlme)
library(aomisc)
require(dplyr)
require(ggplot2)
require(ggExtra)
require(cowplot)
require(igraph)

correct_delta_value <- function(data_to_use){
  
  # fix values
  data_to_use$delta_normalised_run_time <-c(data_to_use$delta_normalised_run_time[2:(length(data_to_use$delta_normalised_run_time))], 0)
  
  # return
  return(data_to_use)
}

calculate_record_duration <- function(data_to_use){
  
  # calculate record duration
  data_to_use$record_duration <- c(0, data_to_use$normalised_run_date[1:(length(data_to_use$normalised_run_date) - 1)] - data_to_use$normalised_run_date[2:length(data_to_use$normalised_run_date)])
  
  # return
  return(data_to_use)
  
}

obtain_time_window_with_max_delta <- function(data_to_use){
  
  # try when highest delta was set
  data_to_use$max_delta_num_tries <- which.max(data_to_use$delta_normalised_run_time)[1]
  # value of largest delta
  data_to_use$largest_delta <- max(data_to_use$delta_normalised_run_time)
  # time when largest delta was set
  data_to_use$run_date_largest_delta <- data_to_use$normalised_run_date[which.max(data_to_use$delta_normalised_run_time)[1]]
  
  # return data
  return(data_to_use) 
}


data <- read.csv('clean_world_record_data.csv', row.names = 1)

data_record_duration <- data %>% 
  group_by(record_compound_id) %>%
  group_modify(~calculate_record_duration(.))

data_record_duration <- data_record_duration %>%
  filter(record_duration > 0)

data_delta <-  data %>% 
  group_by(record_compound_id) %>%
  group_modify(~correct_delta_value(.))

data_delta <- data_delta %>%
  filter(delta_normalised_run_time > 0)

data_clean_largest_delta <- data_delta %>% 
  group_by(record_compound_id) %>%
  group_modify(~obtain_time_window_with_max_delta(.)) 

data_max_delta_time_window <- data_clean_largest_delta %>% 
  distinct(record_compound_id, .keep_all= TRUE)


figure_A <- ggplot(data, aes(x = normalised_run_date, y = normalised_run_time)) + 
  geom_line(aes(group = record_compound_id), size = 0.02) +
  theme_minimal() + xlab('Run date') + 
  ylab('WR Time') + theme(aspect.ratio = 1)


figure_B <- ggplot(data_delta, aes(x = normalised_run_date, y = delta_normalised_run_time)) + 
  geom_point(size = 0.02) +
  theme_minimal() + xlab('Run date') + 
  ylab('Delta WR Time') + theme(aspect.ratio = 1) + geom_smooth(method = 'lm')

figure_B <- ggMarginal(figure_B, type="histogram", size=10, alpha = 0.7)


figure_D <- ggplot(data = data_max_delta_time_window, aes(x = run_date_largest_delta, y = largest_delta)) +
  geom_point(size = 0.02) +
  theme_minimal() + xlab('Run date') + 
  ylab('Largest Delta WR Time') + theme(aspect.ratio = 1) + geom_smooth(method = 'lm')

figure_D <- ggMarginal(figure_D, type="histogram", size=10, alpha = 0.7)

figure_4 <- ggplot(data_record_duration, aes(x = normalised_run_date, y = record_duration)) + 
    geom_point(size = 0.02) +
    theme_minimal() + xlab('Run date') + 
    ylab('WR duration') + theme(aspect.ratio = 1) + geom_smooth(method = 'lm')

figure_4 <- ggMarginal(figure_4, type="histogram", size=10, alpha = 0.7)

figure_C <- ggplot(data_record_duration, aes(x = delta_normalised_run_time, y = record_duration)) + 
  geom_point(size = 0.02) +
  theme_minimal() + xlab('Delta WR Time') + 
  ylab('WR duration') + theme(aspect.ratio = 1) + geom_smooth(method = 'lm')

figure_C <- ggMarginal(figure_C, type="histogram", size=10, alpha = 0.7)

figure_1 <- plot_grid(figure_A, figure_B, figure_C, figure_D, labels = 'AUTO')

## NETORK ANALYSIS

convert_raw_data_to_network <- function(data_to_process){
  
  # Extract names of player and record
  gamer_id <- unique(data_to_process[,2])
  record_id <- unique(data_to_process[,1])
  
  # create matrix of zeros
  incidence_mat <- matrix(0, nrow = length(gamer_id), ncol = length(record_id))
  rownames(incidence_mat) <- gamer_id
  colnames(incidence_mat) <- record_id
  
  # fill incidence matrix
  for (i in 1:nrow(data_to_process)) {
    
    incidence_mat[data_to_process[i,2],data_to_process[i,1]] <-  incidence_mat[data_to_process[i,2],data_to_process[i,1]] + 1
    
  }
  
  return(incidence_mat)
  
}

# Build bipartite network from raw data
network_record_level <- data %>% 
  select(record_compound_id, run_player_id) %>%
  convert_raw_data_to_network(.) %>%
  graph_from_incidence_matrix(., weighted = TRUE)

# Calculate node descriptors
degree_values <- degree(network_record_level)
names_gamer_and_game <- names(degree_values)
type_node <- as.numeric(V(network_record_level)$type)
betweenness_centrality_values <- betweenness(network_record_level)

network_clean_data <- as.data.frame(cbind(names_gamer_and_game, type_node, unname(degree_values), unname(betweenness_centrality_values)))
colnames(network_clean_data) <- c('name_id', 'node_type', 'degree', 'betweenness_centrality')

player_network_clean_data <- network_clean_data %>% filter(node_type == 0)
figure_E <- ggplot(data = player_network_clean_data, aes(x =  as.numeric(degree))) + geom_histogram(color = 'black', alpha = 0.6) + theme_minimal() +
  xlab('Degree') + ylab('Frequency') +  ggtitle('Player Degree Distribution') + theme(aspect.ratio = 1)

game_network_clean_data <- network_clean_data %>% filter(node_type == 1)
figure_F <- ggplot(data = game_network_clean_data, aes(x =  as.numeric(degree))) + geom_histogram(color = 'black', alpha = 0.6) + theme_minimal() + 
  xlab('Degree') + ylab('Frequency') + ggtitle('Game Degree Distribution')  + theme(aspect.ratio = 1)

figure_2 <- plot_grid(figure_E, figure_F, labels = 'AUTO')

player_network_clean_data_with_delta <- data_delta %>% left_join(player_network_clean_data, by = c("run_player_id" = "name_id"))
summarised_player_network_clean_data_with_delta <- player_network_clean_data_with_delta %>% group_by(degree) %>% 
  summarise(mean_delta_normalised_run_time = mean(delta_normalised_run_time), 
            se_mean_delta_normalised_run_time = sd(delta_normalised_run_time)/sqrt(length(delta_normalised_run_time)),
            mean_normalised_run_date = mean(normalised_run_date),
            se_mean_normalised_run_date = sd(normalised_run_date)/sqrt(length(normalised_run_date))) 

player_network_clean_data_with_max_delta <- data_max_delta_time_window %>% left_join(player_network_clean_data, by = c("run_player_id" = "name_id"))

player_network_clean_data_with_duration <- data_record_duration %>% left_join(player_network_clean_data, by = c("run_player_id" = "name_id"))
summarised_player_network_clean_data_with_duration <- player_network_clean_data_with_duration %>% group_by(degree) %>% 
  summarise(mean_record_duration = mean(record_duration), 
            se = sd(record_duration)/sqrt(length(record_duration))) 

figure_F <- ggplot(data = player_network_clean_data_with_delta, aes(x = as.numeric(degree), y = delta_normalised_run_time)) + 
  geom_point(size = 0.02) + theme_minimal() + xlab('Player degree') + ylab('Delta WR Time') + theme(aspect.ratio = 1) +
  geom_smooth(method = 'lm')

figure_F2 <- ggplot(data = summarised_player_network_clean_data_with_delta,
       aes(x = as.numeric(degree), y = mean_delta_normalised_run_time)) + 
  geom_pointrange(data = summarised_player_network_clean_data_with_delta,
                  aes(x = as.numeric(degree), y = mean_delta_normalised_run_time,
                  ymin = mean_delta_normalised_run_time - se_mean_delta_normalised_run_time, 
                  ymax = mean_delta_normalised_run_time + se_mean_delta_normalised_run_time)) + geom_smooth(method = 'lm') + 
  theme_minimal() + theme(aspect.ratio = 1) + xlab('Player degree') + ylab('Delta WR Time')

figure_F3 <- ggplot(data = summarised_player_network_clean_data_with_delta,
                    aes(x = as.numeric(degree), y = mean_normalised_run_date)) + 
  geom_pointrange(data = summarised_player_network_clean_data_with_delta,
                  aes(x = as.numeric(degree), y = mean_normalised_run_date,
                      ymin = mean_normalised_run_date - se_mean_normalised_run_date, 
                      ymax = mean_normalised_run_date + se_mean_normalised_run_date)) + geom_smooth(method = 'lm') + 
  theme_minimal() + theme(aspect.ratio = 1) + xlab('Player degree') + ylab('Mean run date')

ggplot(data = player_network_clean_data_with_delta, aes(x = as.numeric(degree), y = normalised_run_date)) + 
  geom_point(size = 0.02) + theme_minimal() + xlab('Player degree') + ylab('Delta WR Time') + theme(aspect.ratio = 1) +
  geom_smooth(method = 'lm')

figure_G <- ggplot(data = player_network_clean_data_with_max_delta, aes(x = as.numeric(degree), y = delta_normalised_run_time)) + 
  geom_point(size = 0.02) + theme_minimal() + xlab('Player degree') + ylab('Largest Delta WR Time') + theme(aspect.ratio = 1) +
  geom_smooth(method = 'lm')

ggplot(data = player_network_clean_data_with_max_delta, aes(x = as.numeric(degree), y = run_date_largest_delta)) + 
  geom_point(size = 0.02) + theme_minimal() + xlab('Player degree') + ylab('Delta WR Time') + theme(aspect.ratio = 1) +
  geom_smooth(method = 'lm')

aa <- player_network_clean_data_with_delta %>% group_by(degree) %>%
  summarise(count_delta = n())

bb <- player_network_clean_data_with_max_delta %>% group_by(degree) %>%
  summarise(count_max_delta = n()) %>% 

cc <- aa %>% left_join(bb) %>% mutate(prob = count_max_delta/count_delta)
cc[is.na(cc)] <- 0


figure_H <- ggplot(data = cc, aes(x = as.numeric(degree), y = prob)) + 
  geom_point() + theme_minimal() + xlab('Player degree') + ylab('Fraction of times WR was most impactful') + theme(aspect.ratio = 1) +
  geom_smooth(method = 'lm')

xx <- player_network_clean_data_with_duration %>% 
  group_by(degree) %>% 
  summarise(count_duration = n())

zz <- player_network_clean_data_with_duration %>% 
  group_by(record_compound_id) %>% 
  filter(record_duration == max(record_duration))

yy <- zz %>% 
  group_by(degree) %>% 
  summarise(count_max_duration = n())

cc2 <- xx %>% left_join(yy) %>% mutate(prob = count_max_duration/count_duration)
cc2[is.na(cc2)] <- 0

figure_H2 <- ggplot(data = cc2, aes(x = as.numeric(degree), y = prob)) + 
  geom_point() + theme_minimal() + xlab('Player degree') + ylab('Fraction of times WR was longest lasting') + theme(aspect.ratio = 1) +
  geom_smooth(method = 'lm')

figure_I <- ggplot(data = player_network_clean_data_with_duration, aes(x = as.numeric(degree), y = record_duration)) + 
  geom_point(size = 0.02) + theme_minimal() + xlab('Player degree') + ylab('Record Duration') + theme(aspect.ratio = 1) +
  geom_smooth(method = 'lm')

figure_I2 <- ggplot(data = summarised_player_network_clean_data_with_duration,
                    aes(x = as.numeric(degree), y = mean_record_duration)) + 
  geom_pointrange(data = summarised_player_network_clean_data_with_duration,
                  aes(x = as.numeric(degree), y = mean_record_duration,
                      ymin = mean_record_duration - se, 
                      ymax = mean_record_duration + se)) + geom_smooth(method = 'lm') + 
  theme_minimal() + theme(aspect.ratio = 1) + xlab('Player degree') + ylab('Mean record duration')


figure_3 <- plot_grid(figure_F, figure_H, figure_I, labels = 'AUTO', nrow = 1)
figure_3V2 <- plot_grid(figure_F3, figure_H, figure_H2, figure_F2, figure_I2, labels = 'AUTO', nrow = 3)
plot_grid(figure_F3, figure_H, figure_H2, labels = 'AUTO', nrow = 1)

game_network_clean_data_with_delta <- data_delta %>% left_join(game_network_clean_data, by = c("record_compound_id" = "name_id"))
game_network_clean_data_with_max_delta <- data_max_delta_time_window %>% left_join(game_network_clean_data, by = c("record_compound_id" = "name_id"))
game_network_clean_data_with_duration <- data_record_duration %>% left_join(game_network_clean_data, by = c("record_compound_id" = "name_id"))


ggplot(data = game_network_clean_data_with_delta, aes(x = (as.numeric(degree)), y = delta_normalised_run_time)) +
  geom_smooth(method = 'lm') + geom_point() + theme_minimal() + theme(aspect.ratio = 1) + xlab('Game Degree') +
  ylab('Delta WR Time')

ggplot(data = game_network_clean_data_with_max_delta, aes(x = (as.numeric(degree)), y = max_delta_num_tries)) +
 geom_jitter() + theme_minimal() + theme(aspect.ratio = 1) + geom_smooth(method = 'lm') + xlab('Game Degree') + 
 ylab('Number of attempts until largest \nDelta WR Time was set')

ggplot(data = game_network_clean_data_with_duration, aes(x = (as.numeric(degree)), y = record_duration)) +
  geom_jitter() + theme_minimal() + theme(aspect.ratio = 1)  + xlab('Game Degree') + 
  ylab('Record Duration')
