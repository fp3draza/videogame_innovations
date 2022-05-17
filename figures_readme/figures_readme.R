setwd('~/videogame_innovations')
fit_data <- read.csv('./data/processed/fit_data.csv', row.names = 1)
df_res <- read.csv('./data/processed/fit_res.csv', row.names = 1)
df_res_filtered <- filter(df_res,(tau0_err/tau0 <0.4) & (R_sq_adj > 0.65))
df_tmp <- fit_data %>% left_join(., df_res_filtered, by = c("id" = "id")) 
#
legend <- data.frame(
  x = 4.5,
  y = 0.38,
  label = "1/e"
)
#
lambda <- 1
I_inf <- 0 
beta <- 1
#
my_ids <- (filter(df_tmp, tau0_err/tau0 <0.4) %>% distinct(id))$id
#
ggplot(filter(df_tmp,id %in% my_ids), aes(x = (run_date_in_days/tau0)**beta, y = normalized_decay(run_time_percentage, I_inf), col = as.factor(id))) + 
  geom_point(size = 0.5) + 
  stat_function(fun = function(x) stretched_exp(x,lambda,beta,I_inf), color ="black", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) {exp(-1)}, color ="grey", linetype = "dotted", size = 0.5) +
  theme_minimal() + 
  xlab(TeX("$(t/\\tau_0)^\\beta$")) + ylab('percentage of time first record') +
  theme(aspect.ratio = 1,legend.position="none")  +   
  ylim(c(0,1))  + xlim(c(0,5)) +   
  geom_text(data=legend, aes( x=x, y=y, label=label),
            color="black",
            size=3.5, angle=0) 

