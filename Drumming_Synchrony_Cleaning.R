rm(list=ls())
user <- "SM"
#user <- "AL"

if(user == "SM"){
  data_dir <- "C:\\Users\\mcwee\\OneDrive - McMaster University\\Social Drumming\\REAPER_trial_data\\"
  setwd("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\")
  fun_dir <- paste0(getwd(), "/functions")
} else if(user == "AL"){
  setwd("~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation")
  data_dir <- "~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation/Data/"
} else if(user == "YAS"){
  data_dir <- ""
}

library(tidyverse)
library(rlang)
library(zoo)
library(ggplot2)
library(dtw)
list.files(fun_dir, full.names = TRUE) %>% walk(source)

###TASK###
# 3Put expected pulse for synch condition from kick and snare and deviation
metronome <- tibble(start_s = seq(1,32,1),
                    participant = rep(3, 32))

# Importing
dyad <- 206
trial <- 4

data <- load_data(dyad, trial)
data <- flip_participants(data)
data <- remove_double_hits(data)
data <- align_first_sync_hit(data)
data <- trim_end(data)
data <- recalc_onsets(data)

gg_s(data)

sk1 <- sum(data$skip_flag, na.rm = T)
if(sk1 >= 1){
  for(i in 1:sk1){
    data <- InsertMissedHit(data, 1)
    data <- recalc_onsets(data)
  }
}

gg_s(data)

sk2 <- sum(data$double_skip_flag, na.rm = T)
if(sk2 >= 1){
  for(i in 1:sk2){
    data <- InsertMissedHit(data, 2)
    data <- recalc_onsets(data)
  }
}
gg_s(data)

generate_stats(data)
