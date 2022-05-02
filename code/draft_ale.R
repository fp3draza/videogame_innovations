require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)

setwd('~/videogame_innovations/code')
data <- read.csv('../speedrun_data_clean.csv', row.names = 1)
source("helper.R")

ids <- filter(data, run_date_in_seconds > 1000)  %>% distinct(id)  #%>% filter(., run_time_percentage < 90)
ids1 <- filter(data, run_date_in_seconds > 3600) %>% filter(., run_time_percentage < 25) %>% distinct(id) 
mydata1 <- filter(data, id %in% ids1$id)

dim(ids)
dim(distinct(data,id))
dim(data)


rows_per_id <- data %>% count(id) %>% filter(n >10) 

dim(rows_per_id)

rows_per_id %>% formattable()
ids <- filter(ids, !(id %in% ids1$id))

# mydata <- filter(data, id %in% sample(ids$id, replace= TRUE, 10))

mydata <- filter(data, id %in% sample(rows_per_id$id, replace= TRUE, 4))

mydata %>% distinct(id)

# ggplot(mydata, aes(x = run_date_in_seconds, y = log(run_time))) + geom_line(aes(group = id), size = 0.07)  + 
#   geom_point(size = 0.5) + 
#   theme_minimal() + xlab('days since first record') + ylab('time first record') + 
#   theme(aspect.ratio = 1) 
#   #scale_y_continuous(trans='log') +
  #ylim(-2, 0.1) # + xlim(0, 1)


my_id <- "yo1yv1q54xk906k0game-level" #   "3dxkz0v19kv83mjdgame-level"

ggplot(filter(mydata,id == "3dxkz0v19kv83mjdgame-level"), aes(x = run_date_in_seconds, y = log(run_time_percentage/100))) + geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) power_law(x,-2e-1,0.2), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('percentage of time first record') + 
  theme(aspect.ratio = 1) 

ggplot(filter(mydata,id == "yo1yv1q54xk906k0game-level"), aes(x = run_date_in_seconds, y = log(run_time_percentage/100))) + geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) power_law(x,-4e-5,1.2), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('percentage of time first record') + 
  theme(aspect.ratio = 1) 


