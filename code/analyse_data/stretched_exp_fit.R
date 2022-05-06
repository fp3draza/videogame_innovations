require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)

setwd('~/videogame_innovations')
data <- read.csv('./clean_data/speedrun_data_clean.csv', row.names = 1)
source("./code/analyse_data/helper.R")

colnames(data)

# complete dataset
dim(data)

# extract the maximum time per game id 
df_run_date_max <- data %>% group_by(id) %>% summarize(run_date_in_days_max = max(run_date_in_days))

# choose time series containing more than 9 points  

rows_per_id <- data %>% count(id) %>% filter(n >8) 
dim(rows_per_id)
rows_per_id %>% formattable()

# restrict our dataset to the selected ids
my_data <- filter(data, id %in% rows_per_id$id)

distinct(my_data, id)

# Data linearization for stretched-exponential decay

df2 <- my_data %>% select(id, run_date_in_days, run_time_percentage) 
df1 <- df2 %>% mutate(log_improvement=log(run_time_percentage/100)) %>%
  filter(log_improvement<0) %>% 
  mutate(log_log_improvement=log(-log_improvement),
         log_time = log(run_date_in_days))

#ids <- sample(df1$id, replace= TRUE, 15)

# match("9d3rvzwdmke9evjdgame-level",rows_per_id$id) # has only run_date_in_days = 1 
# match("9d3rvzwd7kj3q1x2game-level",rows_per_id$id)


# filter(df1, id %in% rows_per_id[48:53,]$id)
# match("9d3rvzwd7kj3q1x2game-level",ids)


# remove problematic data sets
bad_ids <- c("9d3rvzwdmke9evjdgame-level",
             "9do8jge1jdzlomr2game-level",
             "lde3r2l6ndx41ev2game-level",
             "pd0qq4v1824le9gdgame-level",
             "yd478gde5dw43j0k29vgzgl9",
             "yd478gde5dw43j0k592m4m3w",
             "pd0wpq21ndx79m12xd01rljw")
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
  tau0 <- exp(-beta0/beta1)
  p_val <- summary(fit)$coeff[2,4]
  R_sq <- summary(fit)$r.squared
  R_sq_adj <- summary(fit)$adj.r.squared
  
  
  if (is.na(beta1)){
    print(paste0("ACHTUNG we have a problem at id = ", my_id)) 
    print(summary(fit))
    break
  } 
  
  row <- data.frame(my_id, beta0, beta1, tau0, R_sq, R_sq_adj, p_val)
  
  print(row)
    
  df_fit <- rbind(df_fit, row)
    
}

colnames(df_fit) <- c("id", "beta0", "beta1", "tau0","R_sq", "R_sq_adj", "p_val")

dim(df_fit)
df_fit %>% formattable()

# add the column run_date_in_days_max to df_fit
df_fit <- df_fit %>% left_join(., df_run_date_max, by = c("id" = "id")) 

# Analysis based on the adjusted R-square value  
# precetage of dataexplained better than a constant https://people.duke.edu/~rnau/rsquared.htm

# negative_R_sq_ids <- filter(df_fit, R_sq_adj < 0)$id
# bad_fit_ids <- (filter(df_fit, R_sq_adj > 0) %>%  filter(., R_sq_adj < 0.5))$id
# good_ids <- (filter(df_fit, R_sq_adj > 0.93) %>% filter(beta1>1))$id
# good_ids <- (filter(df_fit, R_sq_adj > 0.9))$id

#  
selected_tmp <- filter(df_fit, (tau0 >1) & (tau0 < 5*run_date_in_days_max)) %>% # & (R_sq_adj > 0.6)) %>%
  select(id, R_sq, R_sq_adj,tau0, run_date_in_days_max) #%>% head(20)


exp(-1./5)

dim(selected_tmp)

selected_tmp %>% formattable()

selected_ids <- (filter(df_fit, (R_sq_adj > 0.6) & (tau0 >1) & (tau0<tau_max) ))$id
length(selected_ids)
filter(df_fit, id %in% selected_ids) %>% formattable()


# DISPLAY HISTOGRAMS for fitted parameters 
# beta1 
ggplot(filter(df_fit, id %in% selected_ids), aes(x=beta1)) +
  geom_histogram() + geom_density(color="red") + 
  xlim(0,3.2)

filter(df_fit, (beta1>2) & (beta1<3) & (id %in% selected_ids)) # not even one between 3 and 4
filter(df_fit, (beta1>4) & (id %in% selected_ids)) # => sudden drop sigmoid like 

# tau0
ggplot(filter(df_fit, id %in% selected_ids), aes(x=tau0)) +
  geom_histogram(binwidth=1000, boundary = 0, color="blue") 

filter(df_fit, (tau0>1000) & (tau0<2000) & (id %in% selected_ids)) 

dim(filter(df_fit, (tau0<=1000) & (id %in% selected_ids))) # 138/221


# create categorical variables 
df_res <- filter(df_fit, id %in% selected_ids) %>% mutate(beta1_cat = beta_classifier_V(beta1),
                                               tau0_cat = tau_classifier_V(tau0))

df_res %>% formattable() 

dim(df_res). # 221 => 166 (train) + 55 (test) 

# OSS 
# we could consider creating less categories for tau maybe just smaller/larger than 1000 days (138/221)
# to be fully consistent we should also check for tmax > tau0 for a given fitting 
# but we would probably through away many other points    
# we could join my_data and df_fit on id and plot some reduced plot vs time/tau0
# at least those associated with a large R-squared 
# non-linear fitting would be more appropriate to include a lower bound tot he possible improvement 
# which with the current model is unrealistically zero, i.e., 
# with the stretched-exp decay we are assuming that the run_time could be brought to zero after an infinite number of attempts. 


# VISUALIZE the fit for a selected id== my_id on the original dataset

#my_id <-"y65r341e824xo0edkwj75xzw" # "kdkxgg6m9kvmwxokgame-level" # "k6qgnmdg02ql5m9kgdre30e9"

# staircase "268exwy6vdo31v9dgame-level"  

my_id <- "268erm76vdoxgl9dgame-level" 

filter(df_fit, id==my_id)$beta0
lambda <- exp(filter(df_fit, id==my_id)$beta0)
beta1 <- filter(df_fit, id==my_id)$beta1

# to plot rnd selected fit from the batch selected_ids
#ggplot(filter(my_data, id %in% sample(selected_ids, replace= TRUE, 10), aes(x = run_date_in_days, y = run_time_percentage/100)) + geom_line(aes(group = id), size = 0.07,color="blue") + 
ggplot(filter(my_data,id == my_id), aes(x = run_date_in_days, y = run_time_percentage/100)) + geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) stretched_exp(x,lambda,beta1,0.), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('percentage of time first record') + 
  theme(aspect.ratio = 1) #+   xlim(c(1000,1100)) 

# visualized linearized fit 

d <-df1 %>% filter(id == my_id) %>% select(id, log_time, log_log_improvement)
fit <- lm(log_log_improvement ~ log_time, data = d) # fit the model
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values

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


##########################################################

