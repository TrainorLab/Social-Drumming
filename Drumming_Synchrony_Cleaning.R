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

# Importing
dyads <- 201:212
trials <- 1:4
# dyad <- 201
# trial <- 4

for (dyad in dyads){
  for(trial in trials){
    data <- load_data(dyad, trial)
    data <- flip_participants(data)
    data <- remove_double_hits(data)
    data <- align_first_hit(data)
    data <- trim_end(data)
    data <- recalc_onsets(data)
    
    gg_s(data)
    
    tryCatch(
      {
        data <- clean_all_missed(data)
        gg_s(data)
      },
      error = function(e) {
        message("An error occurred: ", conditionMessage(e))
        data <<- NULL
      },
      warning = function(w) {
        # Code to handle warnings if required
        message("A warning occurred: ", conditionMessage(w))
        # Additional actions or warning handling if needed
      }
    )
    
    
    if(dyad == 203 & trial == 3){
      data <- remove_hit(data, 1, 76)
    }
    
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

