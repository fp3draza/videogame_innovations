require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)

setwd('~/videogame_innovations')
data <- read.csv('./data/raw/speedrun_data_clean.csv', row.names = 1)

source("./code/analyse_data/helper.R")
source("./code/analyse_data/functions_fit.R")

write_output_filename <-'./data/processed/fit_res.csv'
write_fitdata_filename <-'./data/processed/fit_data.csv'

dim(data)
dim(data_clean)
colnames(data_clean_old)

# complete dataset ask Fernando whats the difference between 13 vs 8 column 
# plots mad with data_clean_old (that is probably equal to data_clean)

# remove improvements occurred on day 1
data_days <- data %>% filter(!((run_date_in_days == 1) & (run_time_percentage != 100)))
dim(data_days)

# extract the maximum time per game id 
df_run_date_max <- data_days %>% group_by(id) %>% summarize(run_date_in_days_max = max(run_date_in_days))

# choose time series containing more than 9 points  
rows_per_id <- data_days %>% count(id) %>% filter(n >8) 

# estimate of the percentage of curve with more than 12 points 
dim(rows_per_id)
dim(filter(rows_per_id, n>12))/dim(rows_per_id)

rows_per_id %>% formattable()

# remove problematic data sets
bad_ids <- c("9d3rvzwdmke9evjdgame-level",
             "9do8jge1jdzlomr2game-level",
             "lde3r2l6ndx41ev2game-level",
             "pd0qq4v1824le9gdgame-level",
             "yd478gde5dw43j0k29vgzgl9",
             "yd478gde5dw43j0k592m4m3w",
             "pd0wpq21ndx79m12xd01rljw",
             "369p7el1zd3wng8kgame-level",
             "369pqq315dw66og2game-level",
             "o6g08xd2wdmxl6okgame-level")

ids <- filter(rows_per_id, !(id %in% bad_ids))$id

# restrict our dataset to the selected ids
my_data <- filter(data_days, id %in% ids)

distinct(my_data, id)

# save data fitted in a file
write.csv(my_data, write_fitdata_filename)

#  start non-linear fit 

df_fit <- NULL
length(ids)
ids[-1:-19]
tail(ids,3)

iter_max <- 10000


for (my_id in ids) {
  
  print(paste0("id =", my_id))
  
  d <-my_data %>% filter(id == my_id) %>% 
    mutate(run_time_improvement = run_time_percentage/100) %>%
    select(run_date_in_days, run_time_improvement)

  # initial guess of predictors
  I_inf<- min(d$run_time_improvement)
  lambda <- -log(I_inf)/max(d$run_date_in_days)  
  beta <- 1.
  
  row <- stretched_exp_fit(d, lambda, beta, I_inf, iter_max) %>% 
    mutate(id=my_id)
  
  print(row)
  
  df_fit <- rbind(df_fit, row)  
  
}

dim(df_fit)
colnames(df_fit)

dim(df_fit  %>% filter((convergence == TRUE)) %>% 
  select(id, beta, tau0, tau0_err,R_sq_adj, delta_R_sq, iter, convergence))

# add the column run_date_in_days_max to df_fit
df_fit <- df_fit %>% left_join(., df_run_date_max, by = c("id" = "id")) 
dim(df_fit)

# R-square value precetage of dataexplained better than a constant https://people.duke.edu/~rnau/rsquared.htm

# filter w.r.t. tau0 > t_max & create categorical variables for R_sq_adj, beta, and tau0
df_res <- df_fit %>% filter(.,tau0 < 5*run_date_in_days_max) %>% 
  mutate(R_sq_cat = trunc(10*R_sq_adj,0), 
                         beta_cat = if_else(beta<3, trunc(beta), 3),
                         tau0_cat = if_else(tau0<3.5e3, trunc(tau0/500), -1))
dim(df_res)

# save dataframe into a file
write.csv(df_res, write_output_filename)
