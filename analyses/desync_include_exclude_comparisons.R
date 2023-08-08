rm(list = ls())
trial_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_df.rds")
avg_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_avgs_df.rds")
beh <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_df.rds")


trial_df_no_desync <- trial_df %>% filter(desync_events == 0)

avg_df_no_desync <- trial_df_no_desync %>%
  group_by(Dyad, ID) %>%
  summarize(
    n_clean_trials = n(),
    desyncs = sum(desync_events, na.rm = T),
    pairwise_async_mean = mean(pairwise_async),
    onset_async_mean = mean(onset_async),
    ac1_async_mean = mean(ac1_async),
    detrend_ac1_async_mean = mean(detrend_ac1_async),
    ac1_ITI_mean = mean(ac1_ITI),
    detrend_ac1_ITI_mean = mean(detrend_ac1_ITI),
    cont_bpm = mean(cont_bpm),
    n_imputed = mean(n_imputed),
    clean_hits = mean(clean_hits),
    clean_hits2 = mean(clean2),
    ITI_var_1p = mean(ITI_var_1p),
    ITI_var_2p = mean(ITI_var_2p),
    log_ITI_var_1p = mean(log_ITI_var_1p),
    log_ITI_var_2p = mean(log_ITI_var_2p),
    ITI_var_1p_detrend = mean(ITI_var_1p_detrend),
    ITI_var_2p_detrend = mean(ITI_var_2p_detrend),
    log_ITI_var_1p_detrend = mean(log_ITI_var_1p_detrend),
    log_ITI_var_2p_detrend = mean(log_ITI_var_2p_detrend),
    n_included_trials = n()
  )

avg_df_no_desync$n_included_trials - avg_df$n_included_trials

avg_df$desyncs
sum(avg_df$n_included_trials == 1)
sum(avg_df$n_included_trials)


sum(avg_df_no_desync$n_included_trials == 1)
sum(avg_df_no_desync$n_included_trials)




ggplot(trial_df, aes(x = factor(desync_events), y = detrend_ac1_ITI, fill = factor(desync_events))) +
  geom_boxplot() +
  theme_bw()

ggplot(trial_df, aes(x = factor(desync_events), y = log_ITI_var_1p_detrend, fill = factor(desync_events))) +
  geom_boxplot()  +
  theme_bw()

ggplot(trial_df, aes(x = factor(desync_events), y = log_ITI_var_2p_detrend, fill = factor(desync_events))) +
  geom_boxplot()  +
  theme_bw()

