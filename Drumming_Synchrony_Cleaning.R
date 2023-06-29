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
dyads <- c(101:119, 201:211)
trials <- 1:4
dyad <- 102
trial <- 2

start <- Sys.time() 
for (dyad in dyads){
  for(trial in trials){
    print(paste0(dyad, ", ", trial))
    data <- load_data(dyad, trial)
    data <- flip_participants(data)
    data <- remove_double_hits(data)
    data <- align_first_hit(data)
    data <- trim_end(data)
    data <- modify_individual_trial(data)
    data <- recalc_onsets(data)
    gg_s(data)
    
    if(dyad > 200){
      
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
      
    }
    
    tryCatch(
      {
        data <- detrend_cont(data)
      },
      error = function(e) {
        message("An error occurred: ", conditionMessage(e))
        x <<- NULL
      },
      warning = function(w) {
        # Code to handle warnings if required
        message("A warning occurred: ", conditionMessage(w))
        # Additional actions or warning handling if needed
        data <- detrend_cont(data)
      }
    )    
    
    
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
        x <- generate_stats(data)
      }
    )
    
    
    assign(paste0("trial_", trial), x)
  }

   full_dyad_data <- list(trial_1, trial_2, trial_3, trial_4)
   write_rds(full_dyad_data, paste0("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_sync_output\\", dyad, "_output.rds"))
}
end <- Sys.time()
elapsed <- print(end-start)
