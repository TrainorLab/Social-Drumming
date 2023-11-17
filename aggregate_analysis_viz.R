library(readr)
library(psych)
library(tidyverse)
library(rethinking)
library(bayestestR)
library(BayesFactor)
rm(list = ls())
trial_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_df.rds")
avg_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_avgs_df.rds")
beh <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_df.rds")

beh <- beh %>% filter(Exclude == FALSE)


trial_df$ac1_detrend_diff
hist(trial_df$ac1_detrend_diff)

hist(avg_df$ac1_detrend_diff[seq(1, length(avg_df$ac1_detrend_diff), 2)])


describe(avg_df$ac1_detrend_diff[avg_df$ac1_detrend_diff > 0])
describe(trial_df$ac1_detrend_diff)


avg_df %>% ungroup() %>%
  summarize(mean_log_var_1p_ITI = mean(log_ITI_var_1p),
            sd_log_var_1p_ITI = sd(log_ITI_var_1p),
            mean_ac1 = mean(ac1_ITI_mean),
            sd_ac1 = sd(ac1_ITI_mean),
            mean_ac1_detrend = mean(detrend_ac1_ITI_mean),
            sd_ac1_detrend = sd(detrend_ac1_ITI_mean),
            mean_log_var_2p_ITI = mean(log_ITI_var_2p),
            sd_log_var_2p_ITI = sd(log_ITI_var_2p))



avg_df %>% group_by(condition) %>%
  summarize(mean_log_var_1p_ITI = mean(log_ITI_var_1p),
            sd_log_var_1p_ITI = sd(log_ITI_var_1p),
            mean_log_var_1p_ITI_detrend = mean(log_ITI_var_1p_detrend),
            sd_log_var_1p_ITI_detrend = sd(log_ITI_var_1p_detrend),
            mean_ac1 = mean(ac1_ITI_mean),
            sd_ac1 = sd(ac1_ITI_mean),
            mean_ac1_detrend = mean(detrend_ac1_ITI_mean),
            sd_ac1_detrend = sd(detrend_ac1_ITI_mean),
            mean_log_var_2p_ITI = mean(log_ITI_var_2p),
            sd_log_var_2p_ITI = sd(log_ITI_var_2p))


#boxplot(avg_df$ac1_ITI_mean)
#boxplot(avg_df$log_ITI_var_1p)
#boxplot(avg_df$detrend_ac1_ITI_mean)
hist(avg_df$detrend_ac1_ITI_mean, breaks = seq(-.6, .6, .05))

wide_drum_vars <- avg_df %>%
  group_by(Dyad) %>% 
  dplyr::select(Dyad, ID, log_ITI_var_1p, log_ITI_var_1p_detrend, ac1_ITI_mean, detrend_ac1_ITI_mean) %>%
  pivot_wider(names_from = ID, values_from = c(log_ITI_var_1p, log_ITI_var_1p_detrend, ac1_ITI_mean, detrend_ac1_ITI_mean))

psych::pairs.panels(wide_drum_vars %>% filter(Dyad != 209))

wide_cors <- lowerCor(wide_drum_vars %>% filter(Dyad != 209) %>% ungroup() %>% select(-Dyad))

corPlot(wide_cors)


avg_df_long <- avg_df %>%
  pivot_longer(cols = starts_with("Likert_Q"),
               names_to = "Q",
               values_to = "Likert_Score")

ggplot(avg_df_long, aes(x = Q, y = Likert_Score, fill = ac1_detrend_diff_cat)) +
  geom_boxplot() +
  #geom_dotplot(binaxis = 'y', dotsize=.3, stackdir = 'down') +
  theme_bw()








t.test(detrend_ac1_ITI_mean ~ condition, data = avg_df)
t.test(ac1_ITI_mean ~ condition, data = avg_df)

t.test(log_ITI_var_1p ~ condition, data = avg_df)
t.test(log_ITI_var_1p_detrend ~ condition, data = avg_df)


ITI_var_model <- lm(log_ITI_var_1p_detrend ~ condition, data = avg_df)
ITI_var_resid <- resid(ITI_var_model)

plot(avg_df$log_ITI_var_1p_detrend, ITI_var_resid,
     ylab="Residuals")

hist(ITI_var_resid)






1/ttestBF(formula = ac1_ITI_mean ~ condition, data = avg_df)
ttestBF(formula = detrend_ac1_ITI_mean ~ condition, data = avg_df)

1/ttestBF(formula = log_ITI_var_1p ~ condition, data = avg_df)
ttestBF(formula = log_ITI_var_1p_detrend ~ condition, data = avg_df)





#Calculating Bayes factors with R and JASP Jeffrey Stevens



beh2$condition2 <- ifelse(beh2$condition == "Alternating",1, 0)

m <- rethinking::map(
  alist(
    Likert_Q1 ~ dnorm( mu , sigma ) ,
    mu <- a + b*condition2 ,
    a ~ dnorm(4, 1) ,
    b ~ dnorm(0, 1) ,
    sigma ~ dunif(0, 10)
  ) ,
  data = beh2)

rethinking::precis(m)

extract.samples(m)[1:5,]


m4.3 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 156 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )


beh$condition <- as.factor(beh$condition)

aovBF1 <- anovaBF(Likert_Q1 ~ condition, data = beh)
plot(aovBF1)




##### 
# beh %>%
#   group_by(condition) %>%
#   summarize(mean_Likert1 = mean(Likert_Q1),
#             sd_Likert1 = sd(Likert_Q1),
# 
#             mean_Likert2 = mean(Likert_Q2),
#             sd_Likert2 = sd(Likert_Q2),
# 
#             mean_Likert3 = mean(Likert_Q3),
#             sd_Likert3 = sd(Likert_Q3),
# 
#             mean_Likert4 = mean(Likert_Q4),
#             sd_Likert4 = sd(Likert_Q4),
# 
#             mean_Likert5 = mean(Likert_Q5),
#             sd_Likert5 = sd(Likert_Q5),
# 
#             mean_Likert6 = mean(Likert_Q6),
#             sd_Likert6 = sd(Likert_Q6))



#beh <- beh %>% filter(Dyad < 300)
#####


hist(trial_df$clean_pct)
hist(trial_df$clean2_pct)
hist(avg_df$detrend_ac1_ITI_mean)
hist(avg_df$detrend_ac1_ITI_mean - avg_df$ac1_ITI_mean)

psych::describe(avg_df)

psych::pairs.panels(
  avg_df %>% select(detrend_ac1_ITI_mean, log_ITI_var_1p:log_ITI_var_2p, Likert_Q1:Likert_Q6)%>%
    filter(log_ITI_var_1p < -5)
)


psych::pairs.panels(
  avg_df %>% select(desyncs, Likert_Q1:Likert_Q6))




boxplot(avg_df$log_ITI_var_1p)
hist(avg_df$ITI_var_2p)

idx <- which(avg_df$log_ITI_var_1p < -5)
idx <- which(avg_df$ITI_var_1p < .01)

as.character(avg_df$Dyad[which(avg_df$ITI_var_1p > .01)])

hist(avg_df$log_ITI_var_1p[idx])
hist(avg_df$log_ITI_var_2p[idx])

