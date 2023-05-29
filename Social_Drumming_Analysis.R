### Social Drumming Analysis ###
rm(list=ls())
user <- "SM"
#user <- "AL"

if(user == "SM"){
  data_dir <- "C:\\Users\\mcwee\\OneDrive - McMaster University\\Social Drumming\\REAPER_trial_data\\"
  setwd("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\")
  fun_dir <- ("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\functions\\")
} else if(user == "AL"){
  setwd("~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation")
  data_dir <- "~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation/Data/"
} else if(user == "YAS"){
  data_dir <- ""
}
list.files(fun_dir, full.names = TRUE) %>% walk(source)

library(tidyverse)
library(rlang)
library(zoo)
library(ggplot2)
###TASK###
# 3Put expected pulse for synch condition from kick and snare and deviation
# 2accuracy metric
# 1group by even and odd trial such that participant ID consistent across trials
# 4 use midi as the model of how we want to visualize
#TODO if else to get participant number correct
# Plotting boundaries of end of synch phase and when they drum independent

# Importing

#101_trial 1 bad
dyads <- 101:119
trials <- 1:4
dyad <- 104
trial <- 4

for (dyad in dyads){
  for(trial in trials){
    
    data <- load_data(dyad, trial)
    data <- flip_participants(data)
    data <- remove_double_hits(data)
    data <- recalc_onsets(data)
    data <- align_first_hit(data)
    data <- trim_end(data)
    data <- recalc_onsets(data)
    
    tryCatch(
      {
        x <- generate_stats(data)
      },
      error = function(e) {
        message("An error occurred: ", conditionMessage(e))
        x <<- NULL
      },
      warning = function(w) {
        # Code to handle warnings if required
        message("A warning occurred: ", conditionMessage(w))
        # Additional actions or warning handling if needed
      }
    )
    
    assign(paste0("trial_", trial), x)
  }
  
  full_dyad_data <- list(trial_1, trial_2, trial_3, trial_4)
  write_rds(full_dyad_data, paste0("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_sync_output\\", dyad, "_output.rds"))
}
