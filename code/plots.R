require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)
library(latex2exp)

# histogram of tau0 by beta categories and log-log
my_breaks <- seq(1,11000,200)
df <- NULL

colnames(df_res)
for(my_cat in unique(df_res$beta_cat)){
  my_cat <- 0
  df_tau <- filter(df_res,(R_sq_cat >=7) & (beta_cat == my_cat)) %>% 
    select(id,R_sq_adj,beta,beta_err,tau0,tau0_err) %>% 
    mutate(tau0_rel_err = tau0_err/tau0)
           # tau0_max = tau0+tau0_err,
           # tau0_min = tau0-tau0_err)
  
  h_tau <- hist(df_tau$tau0, freq = TRUE, breaks = my_breaks) 
  # h_tau_max <- hist(df_tau$tau0_max, freq = TRUE, breaks = my_breaks) 
  # h_tau_min <- hist(df_tau$tau0_min, freq = TRUE, breaks = my_breaks) 
   
  df <- rbind(df, data.frame(h_tau$breaks[-length(h_tau$breaks)], h_tau$counts/max(h_tau$counts), my_cat))
}

df_tau %>% formattable() 

df_tmp <- filter(df_tau, tau0_err/tau0 <0.4)
dim(df_tmp)
plot(df_tmp$tau0_rel_err,df_tmp$R_sq_adj)


colnames(df) <- c("tau_intervals", "norm_counts", "beta_cat")

ggplot(filter(df, norm_counts > 0), aes(x = tau_intervals, y = norm_counts, col = as.factor(beta_cat))) + 
  geom_point(size = 1.2) + 
  stat_function(fun = function(x) power_law(x,9.5,-0.75), color ="black", linetype = "solid") +
  theme_minimal() + xlab('tau0') + ylab('nurmalized counts') +
  theme(aspect.ratio = 1) +
  ggtitle("histogram tau0 in log-log scale") + 
  ylim(0.1,1.2) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') #, limits=c(0.01,1.2)) 


# plots of theoretical decay 
lambda <- 1
I_inf <- 0.3
b0 <- 0.5
b1 <- 1
b2 <- 2.5

legend0 <- data.frame(
  x = 3,
  y = 0.7,
  label = paste0("beta = ", b0)
)
#
legend1 <- data.frame(
  x = 3,
  y = 0.8,
  label = paste0("beta = ", b1)
)
#
legend2 <- data.frame(
  x = 3,
  y = 0.9,
  label = paste0("beta = ", b2)
)
#
ggplot() + 
  stat_function(fun = function(x) stretched_exp(x,lambda,b0,I_inf), color ="blue", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) stretched_exp(x,lambda,b1,I_inf), color ="black", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) stretched_exp(x,lambda,b2,I_inf), color ="red", linetype = "solid", size = 0.5) +
  theme_minimal() + 
  theme(aspect.ratio = 1,legend.position="none")  +   
  ylim(c(0,1))  +   xlim(c(0,5))  + 
  geom_text(data=legend0, aes( x=x, y=y, label=label),                  
          color="blue", 
          size=3.5, angle=0) +
  geom_text(data=legend1, aes( x=x, y=y, label=label),                  
          color="black", 
          size=3.5, angle=0) +
  geom_text(data=legend2, aes( x=x, y=y, label=label),                  
          color="red", 
          size=3.5, angle=0) +
  ylab(TeX("$f(t)$")) + 
  xlab(TeX("$t/\\tau_0$"))


# plots of decay collapse

df_tmp <- my_data %>% left_join(., df_res, by = c("id" = "id"))   

my_ids0 <- (filter(df_tmp, (beta > 0.) & (beta < 1.0) & (I_inf > 0.) & (I_inf < 0.1) & tau0_err/tau0 <0.4) %>% distinct(id))$id
beta0 <- mean(filter(df_tmp,id %in% my_ids0)$beta)
I0 <- mean(filter(df_tmp,id %in% my_ids0)$I_inf)
#
my_ids1 <- (filter(df_tmp, (beta > 1) & (beta < 2) & (I_inf > 0.) & (I_inf < 0.1) & tau0_err/tau0 <0.4) %>% distinct(id))$id
beta1 <- mean(filter(df_tmp,id %in% my_ids1)$beta)
I1 <- mean(filter(df_tmp,id %in% my_ids1)$I_inf)
#
my_ids2 <- (filter(df_tmp, (beta > 2) & (beta < 3) & (I_inf > 0.) & (I_inf < 0.1) & tau0_err/tau0 <0.4) %>% distinct(id))$id
beta2 <- mean(filter(df_tmp,id %in% my_ids2)$beta)
I2 <- mean(filter(df_tmp,id %in% my_ids2)$I_inf)

lambda <- 1
I_inf <- I0 # I0 # I1
beta <- beta0 # beta0 # beta1
my_ids <- my_ids0 # my_ids0 # my_ids1

ggplot(filter(df_tmp,id %in% my_ids), aes(x = run_date_in_days/tau0, y = run_time_percentage/100, col = as.factor(id))) + 
  stat_function(fun = function(x) stretched_exp(x,lambda,beta,I_inf), color ="dark grey", linetype = "solid", size = 0.5) +
  geom_point(size = 0.8) + 
  theme_minimal() + 
  xlab('t/tau0') + ylab('percentage of time first record') +
  theme(aspect.ratio = 1,legend.position="none")  +   
  ylim(c(0,1))  +   xlim(c(0,10)) +
  ggtitle("collapse for 0 < beta < 1")  



# plot individual curve
my_id <- "nd2ggq10vdo409o2game-level"
tmp <- filter(df_fit, id %in% my_id) 
lambda <- tmp$lambda
beta <- tmp$beta
I_inf <- tmp$I_inf


ggplot(filter(df_tmp,id == my_id), aes(x = run_date_in_days, y = run_time_percentage/100, col = as.factor(id))) + 
  stat_function(fun = function(x) stretched_exp(x,unique(lambda),unique(beta),unique(I_inf)), color ="purple", linetype = "solid") +
  geom_point(size = 0.8) + 
  theme_minimal() + 
  xlab('t/tau0') + ylab('percentage of time first record') +
  theme(aspect.ratio = 1,legend.position="none")  +   ylim(c(0,1))  +   xlim(c(0,10)) 

#stat_function(fun = function(x) stretched_exp(x,unique(lambda),unique(beta),unique(I_inf)), color ="purple", linetype = "solid") +
#  stat_function(fun = function(x) stretched_exp(x,lambda1,beta1,0.), color ="blue", linetype = "dotted") +