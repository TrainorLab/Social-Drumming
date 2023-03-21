rm(list=ls())
user <- "SM"
#user <- "AL"

if(user == "SM"){
  data_dir <- "C:\\Users\\mcwee\\OneDrive - McMaster University\\Social Drumming\\REAPER_trial_data\\"
  setwd("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\")
} else if(user == "AL"){
  setwd("~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation")
  data_dir <- "~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation/Data/"
} else if(user == "YAS"){
  data_dir <- ""
}
source("InsertMissedHits.R")
source("RemoveDoubleHits.R")
library(tidyverse)
library(rlang)
library(zoo)
library(ggplot2)
library(dtw)
###TASK###
# 3Put expected pulse for synch condition from kick and snare and deviation
# 2accuracy metric
# 1group by even and odd trial such that participant ID consistent across trials
# 4 use midi as the model of how we want to visualize
#TODO if else to get participant number correct
# Plotting boundaries of end of synch phase and when they drum independent

# Importing

#101_trial 1 bad


data <- read.csv(paste0(data_dir, "203_trial2.csv"),stringsAsFactors = T)
names(data) <- c("sel", "mut", "s_ppq",  "e_ppq", "leng", "chan", "pitch", "vel")

# Data Cleaning and Restructuring

# Renaming variables to be readable
# FOR SYNC: NOT 960*2, only 960 (sampling rate)
data <- data %>%
  mutate(s_ppq = (s_ppq/(960)),e_ppq = (e_ppq/(960))) %>%
  rename(start_s = s_ppq,end_s = e_ppq) %>%
  select(-sel, -mut, -chan, -vel)


#adding a participant number to each drummer
data$participant <- ifelse(data$pitch==47,1,2)


#onset difference between 2 participants and individual participants
data$onset_diff_2p <- data$start_s - lag(data$start_s, 1)

data <- data %>% 
  group_by(participant) %>%
  mutate(onset_diff_1p = start_s - lag(start_s, 1))



#Skip flagging mechanisms
# data$flag_skip <- ifelse(data$participant + lag(data$participant,1) == 3,0,1)
# 
# 
# Rolling averages
data <- data %>%
  group_by(participant) %>%
  mutate(roll_1p = rollmean(onset_diff_1p, k=5, align = "right", fill = NA))

data <- data %>% 
  ungroup %>%
  mutate(roll_2p = rollmean(onset_diff_2p, k=5, align = "right", fill = NA))



# Cleaning pt 2 - keep raw start values
data <- data %>%
  select(-end_s, -leng, -pitch)


data <- data %>%
  group_by(participant) %>%
  mutate(hit_number_participant = seq_along(participant))

#data$missed_hit <- ifelse(data$hit_number_participant == lag(data$hit_number_participant, 1) | data$hit_number_participant == lead(data$hit_number_participant, 1), 0, 1)

data$skip_flag <- ifelse(data$onset_diff_1p > 1.5 & data$onset_diff_1p < 2.5, 1, 0)

add_initial_row <- function(data, p, t){
    initial_row <- tibble(participant = p, 
                        start_s = t)
   data <- data %>% 
     ungroup() %>%
     add_row(initial_row, .before = 1)
   
   data <- data %>% group_by(participant)
}
InsertMissedHit <- function(data){
  skips <- which(data$skip_flag==1)
  new_p <- data[skips[1],]$participant
  
  data_cut1 <- data[1:(skips[1] - 1),]
  
  new_row1 <- tibble(participant=new_p,
                     onset_diff_1p=data$roll_1p[skips[1] - 1],
                     roll_1p=data$roll_1p[skips[1] - 1])
  
  
  
  data_cut1 <- data_cut1 %>%
    rows_insert(new_row1, by = names(new_row1))
  
  data_til_end <- data[skips[1]:nrow(data),]
  data_full <- rbind(data_cut1, data_til_end)
}
recalc_onsets <- function(data){
  data <- data %>%
    group_by(participant) %>%
    mutate(hit_number_participant = seq_along(participant))
  
  data <- data %>% 
    group_by(participant) %>%
    mutate(start_s = ifelse(is.na(start_s), onset_diff_1p + lag(start_s), start_s))
  
  data$onset_diff_2p <- data$start_s - lag(data$start_s, 1)
  
  data <- data %>% 
    group_by(participant) %>%
    mutate(onset_diff_1p = start_s - lag(start_s, 1))
  
  data$skip_flag <- ifelse(data$onset_diff_1p > 1.5 & data$onset_diff_1p < 2.5, 1, 0)
  
  return(data)
}
align_first_sync_hit <- function(data){
  idx <- which(data$start_s - lag(data$start_s) < .1) - 1
  data <- data[idx[1]:nrow(data),]
}

data <- align_first_sync_hit(data)
data <- recalc_onsets(data)


for(i in 1:sum(data$skip_flag, na.rm = T)){
  data <- InsertMissedHit(data)
  data <- recalc_onsets(data)
}



# data <- InsertMissedHit(data)
# data <- recalc_onsets(data)


data$participant <- as.factor(data$participant)
data$hit_number <- 1:nrow(data) 

ggplot(data=data, aes(x=start_s, y=onset_diff_1p, group=participant)) +
  geom_line(aes(color=participant))+
  geom_point(aes(color=participant))+
  geom_vline(xintercept = 15,size=1)

ggplot(data=data, aes(x=hit_number_participant, y=onset_diff_1p, group=participant)) +
  geom_line(aes(color=participant))+
  geom_point(aes(color=participant))+
  geom_vline(xintercept = 15,size=1)
