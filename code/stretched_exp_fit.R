require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)

setwd('~/videogame_innovations/code')
data <- read.csv('../speedrun_data_clean.csv', row.names = 1)
source("helper.R")

# complete dataset
dim(data)

# choose time series with more than 10 points  
rows_per_id <- data %>% count(id) %>% filter(n >10) 
dim(rows_per_id)
rows_per_id %>% formattable()

# restrict our dataset to the selected ids
my_data <- filter(data, id %in% rows_per_id$id)

distinct(my_data, id)

# Data linearization 

#my_id <- "3dxkz0v19kv83mjdgame-level" # "yo1yv1q54xk906k0game-level" #   

df2 <- my_data %>% select(id, run_date_in_seconds, run_time_percentage) 
df1 <- df2 %>% mutate(log_improvement=log(run_time_percentage/100)) %>%
  filter(log_improvement<0) %>% 
  mutate(log_log_improvement=log(-log_improvement),
         log_time = log(run_date_in_seconds))

#ids <- sample(df1$id, replace= TRUE, 15)


match("9d3rvzwdmke9evjdgame-level",rows_per_id$id) # has only run_date_in_seconds = 1 
match("9d3rvzwd7kj3q1x2game-level",rows_per_id$id)


# filter(df1, id %in% rows_per_id[48:53,]$id)
# match("9d3rvzwd7kj3q1x2game-level",ids)


# remove problematic data sets
bad_ids <- c("9d3rvzwdmke9evjdgame-level",
             "9do8jge1jdzlomr2game-level",
             "lde3r2l6ndx41ev2game-level",
             "pd0qq4v1824le9gdgame-level",
             "yd478gde5dw43j0k29vgzgl9",
             "yd478gde5dw43j0k592m4m3w")
ids <- filter(rows_per_id, !(id %in% bad_ids))$id

df_fit <- NULL

for (my_id in ids) {

  d <-df1 %>% filter(id == my_id) %>% select(id, log_time, log_log_improvement)
  
  fit <- lm(log_log_improvement ~ log_time, data = d) # fit the model
  d$predicted <- predict(fit)   # Save the predicted values
  d$residuals <- residuals(fit) # Save the residual values

  print("")
  print(paste("now id = ", my_id))

  
  beta0 <- summary(fit)$coeff[1,"Estimate"]
  beta1 <- summary(fit)$coeff[2,"Estimate"]
  p_val <- summary(fit)$coeff[2,4]
  R_sq <- summary(fit)$r.squared
  R_sq_adj <- summary(fit)$adj.r.squared
  
  
  if (is.na(beta1)){
    print(paste0("ACHTUNG we have a problem at id = ", my_id)) 
    print(summary(fit))
    break
  } 
  
  row <- data.frame(my_id, beta0, beta1, R_sq, R_sq_adj, p_val)
  
  print(row)
    
  df_fit <- rbind(df_fit, row)
    
}

colnames(df_fit) <- c("id", "beta0", "beta1", "R_sq", "R_sq_adj", "p_val")

dim(df_fit)
df_fit %>% formattable()

# visualize the fit
ggplot(d, aes(x = log_time, y = log_log_improvement)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "lightgrey") +     # regression line
  geom_segment(aes(xend = log_time, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # color of the points mapped to residual size - green smaller, red larger
  guides(scale = "none") +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

my_id <- "j1l7kedg9kvx31o2game-level"
filter(df1, id==my_id)

negative_R_sq_ids <- filter(df_fit, R_sq_adj < 0)$id

bad_fit_ids <- (filter(df_fit, R_sq_adj > 0) %>%  filter(., R_sq_adj < 0.5))$id

good_ids <- (filter(df_fit, !(id %in% negative_R_sq_ids)) %>% 
               filter(., !(id %in% bad_fit_ids)))$id

good_ids <- (filter(df_fit, R_sq_adj > 0.94))$id

# precetage of dataexplained better than a constant https://people.duke.edu/~rnau/rsquared.htm

selected_ids <- sample(good_ids, replace= TRUE, 10)

ggplot(filter(df1, id %in% good_ids),aes(x = log_time, y = log_log_improvement)) + 
  geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
#  stat_function(fun = function(x) straight_line(x, beta0, beta1), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('log-log percentage of time first record') + 
  theme(aspect.ratio = 1) 







