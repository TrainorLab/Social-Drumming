rm(list = ls())
trial_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_df.rds")
avg_df <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_avgs_df.rds")
beh <- read_rds("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_df.rds")

beh <- beh %>% filter(Exclude == FALSE)


plot_dir <- "C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\good_syncers_plots\\"

scale = 3
png(filename = paste0(plot_dir, "average_trial_metronome_asynchrony.png"), width = 480*scale, height = 480*scale, res = 72*scale)
hist(avg_df$mean_mean_met_async, main = "Average Mean Metronome Asynchrony: Synchronization Phase",
     xlab = "Average Mean Metronome Asynchrony")
dev.off()


png(filename = paste0(plot_dir, "trial_metronome_asynchrony.png"), width = 480*scale, height = 480*scale, res = 72*scale)
hist(trial_df$mean_met_async, main = "Trial Mean Metronome Asynchrony: Synchronization Phase",
     xlab = "Mean Metronome Asynchrony")
dev.off()



wide_met_async <- trial_df %>% select(Dyad, ID, trial, condition, mean_met_async) %>%
  pivot_wider(names_from = ID, values_from = mean_met_async)

wide_met_async2<- trial_df %>% select(Dyad, ID, trial, condition, mean_met_async2) %>%
  pivot_wider(names_from = ID, values_from = mean_met_async2)

wide_met_async$dtx_fix <- ifelse(wide_met_async$Dyad > 212, "After", "Before")
wide_met_async2$dtx_fix <- ifelse(wide_met_async2$Dyad > 212, "After", "Before")


cor.test(wide_met_async$A, wide_met_async$B, use = "pairwise.complete.obs")

wide_met_async_alt <- wide_met_async %>% filter(Dyad < 200)
wide_met_async_sync <- wide_met_async %>% filter(Dyad > 200)

wide_met_async_alt2 <- wide_met_async2 %>% filter(Dyad < 200)
wide_met_async_sync2 <- wide_met_async2 %>% filter(Dyad > 200)


cor.test(wide_met_async_alt$A, wide_met_async_alt$B, use = "pairwise.complete.obs")
cor.test(wide_met_async_sync$A, wide_met_async_sync$B, use = "pairwise.complete.obs")

png(filename = paste0(plot_dir, "scatter_paired_met_asynchronies_by_condition.png"), width = 480*scale, height = 480*scale, res = 72*scale)
ggplot(data = wide_met_async, aes(x = A, y = B, colour = condition)) +
  geom_point() +
#  geom_smooth(method = "lm", se = T) +
  labs(title = "Correlation Between Asynchronies per Trial", x="Participant A", y = "Participant B") + 
  theme_bw()
dev.off()


ggplot(data = wide_met_async2, aes(x = A, y = B, colour = condition)) +
  geom_point() +
  #  geom_smooth(method = "lm", se = T) +
  labs(title = "Correlation Between Asynchronies per Trial: 10 Metronome Hits / Person", x="Participant A", y = "Participant B") + 
  theme_bw()


t1 <- avg_df$mean_mean_met_async2[avg_df$condition == "Synchrony" & as.numeric(as.character(avg_df$Dyad)) >= 213]
t2 <- avg_df$mean_mean_met_async2[avg_df$condition == "Alternating"]

t.test(t1, t2)



ggplot(data = wide_met_async, aes(x = A, y = B, colour = condition)) +
  geom_point() +
  #  geom_smooth(method = "lm", se = T) +
  labs(title = "Correlation Between Asynchronies per Trial", x="Participant A", y = "Participant B") + 
  theme_bw()




png(filename = paste0(plot_dir, "scatter_paired_met_asynchronies_dtx_fix.png"), width = 480*scale, height = 480*scale, res = 72*scale)
ggplot(data = wide_met_async_sync, aes(x=A,y=B,colour = dtx_fix)) +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_point() +
  theme_bw() +
  labs(title = "Synchrony Group: Before and After DTX Setting Change")
dev.off()


ggplot(data = wide_met_async_sync2, aes(x=A,y=B,colour = dtx_fix)) +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_point() +
  theme_bw() +
  labs(title = "Synchrony Group: Before and After DTX Setting Change")





wide_met_async$residual <- wide_met_async$B - wide_met_async$A

ggplot(data = wide_met_async, aes(x = A, y = B, colour = condition)) +
  geom_point() +
  #  geom_smooth(method = "lm", se = T) +
  labs(title = "Correlation Between Asynchronies per Trial", x="Participant A", y = "Participant B") + 
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0) +
  geom_segment(aes(xend = A, yend  = B - residual, colour = condition)) 


print(trial_df %>% select(n_met_async_A, Dyad, trial, valid_metronome), n = 276)
    
    

avg_wide_met_async <- avg_df  %>% select(Dyad, ID, condition, mean_mean_met_async) %>%
  pivot_wider(names_from = ID, values_from = mean_mean_met_async)




ggplot(data = avg_wide_met_async, aes(x = A, y = B, colour = condition)) +
  geom_point() +
  #  geom_smooth(method = "lm", se = T) +
  labs(title = "Correlation Between Asynchronies: Mean", x="Participant A", y = "Participant B") + 
  theme_bw()


cor.test(avg_wide_met_async$A, avg_wide_met_async$B, use = "pairwise.complete.obs")

avg_wide_met_async_alt <- avg_wide_met_async %>% filter(condition == "Alternating")
avg_wide_met_async_sync <- avg_wide_met_async %>% filter(condition == "Synchrony")

cor.test(avg_wide_met_async_alt$A, avg_wide_met_async_alt$B, use = "pairwise.complete.obs")
cor.test(avg_wide_met_async_sync$A, avg_wide_met_async_sync$B, use = "pairwise.complete.obs")






min(wide_met_async_sync$A, na.rm = T)
min(wide_met_async_sync$B, na.rm = T)

min(wide_met_async_alt$A, na.rm = T)
min(wide_met_async_alt$B, na.rm = T)

bad_sync <- (trial_df %>% filter(mean_met_async >= .05))  %>% select(Dyad, ID, trial)
bad_sync_avg <- (avg_df %>% filter(mean_mean_met_async >= .05)) %>% select(Dyad, ID, mean_mean_met_async)

idx <- bad_sync$Dyad == lag(bad_sync$Dyad) & bad_sync$ID == lag(bad_sync$ID)
print(bad_sync[idx,], n = 28)

unique(bad_sync[c("Dyad", "ID")])
unique(bad_sync_avg[c("Dyad", "ID")]) #118 AB 206 AB 208AB 217AB 221AB 222AB, 110B 114B 116A 202A 204B 205A 209A 218B




"%IN%" <- function(x,y){interaction(x) %in% interaction(y)}

bad_sync_avg %>% select(Dyad, ID, mean_mean_met_async)  %IN% unique(bad_sync_avg[c("Dyad", "ID")])

bad_sync_avg
