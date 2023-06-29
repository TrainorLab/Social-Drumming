rm(list = ls())
trial_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_df.rds")
avg_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_avgs_df.rds")
beh <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_df.rds")

beh %>% 
  group_by(condition) %>%
  summarize(mean_Likert1 = mean(Likert_Q1),
            mean_Likert2 = mean(Likert_Q2),
            mean_Likert3 = mean(Likert_Q3),
            mean_Likert4 = mean(Likert_Q4),
            mean_Likert5 = mean(Likert_Q5),
            mean_Likert6 = mean(Likert_Q6))


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
hist(avg_df$ac1_ITI_mean)
hist(avg_df$detrend_ac1_ITI_mean)
hist(avg_df$detrend_ac1_ITI_mean - big_df$ac1_ITI_mean)
psych::pairs.panels(avg_df[,4:ncol(avg_df)])

psych::pairs.panels(
  avg_df %>% select(detrend_ac1_ITI_mean, log_ITI_var_1p:log_ITI_var_2p, Likert_Q1:Likert_Q6)
)


idx <- which(avg_df$ITI_var_1p < .01)

as.character(avg_df$Dyad[which(avg_df$ITI_var_1p > .01)])

hist(avg_df$log_ITI_var_1p[idx])
hist(avg_df$log_ITI_var_2p[idx])
