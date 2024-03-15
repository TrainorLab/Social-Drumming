rm(list=ls())
user <- "SM"
#user <- "AL"
data_dir <- "X:\\Sean M\\Social_Drumming\\REAPER_trial_data\\"

if(user == "SM"){
  setwd("C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\")
  fun_dir <- paste0(getwd(), "/functions")
  plot_dir <- paste0("X:\\Sean M\\Social_Drumming\\beh_sync_output\\trial_plots\\")
} else if(user == "AL"){
  setwd("~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation")
  
} else if(user == "YAS"){
  
}

library(tidyverse)
library(rlang)
library(zoo)
library(ggplot2)
#library(dtw)
list.files(fun_dir, full.names = TRUE) %>% walk(source)

scale = 1

# Importing
dyads <- c(101:122, 
           202:209,
           211:222)
trials <- 1:4
dyad <- 202
trial <- 1

generate_plots <- F

start <- Sys.time() 
for (dyad in dyads){
  for(trial in trials){
    print(paste0(dyad, ", ", trial))
    data <- load_data(dyad, trial)
    data <- flip_participants(data)
    data <- remove_double_hits(data)
    data <- align_first_hit(data)
    data <- trim_end(data)
    if(generate_plots == T){
      png(filename = paste0(plot_dir, dyad,"_",trial,"_init.png"), width = 720*scale, height = 480*scale, res = 72*scale)
      print(gg_s(data, title_mod = ": Aligned, Trimmed, Double Hits Remove - First Pass"))
      dev.off()
    }
    data <- modify_individual_trial(data)
    data <- recalc_onsets(data)
    if(generate_plots == T){
      png(filename = paste0(plot_dir, dyad,"_",trial,"_individual_mod.png"), width = 720*scale, height = 480*scale, res = 72*scale)
      print(gg_s(data, title_mod = ": Post-Individualized Modification", fixed_scale = F))
      dev.off()
    }
    
    if(dyad > 200){
      tryCatch(
        {
          data <- clean_all_missed(data)
        },
        error = function(e) {
          message("An error occurred: ", conditionMessage(e))
          #data <<- NULL
        },
        warning = function(w) {
          # Code to handle warnings if required
          message("A warning occurred: ", conditionMessage(w))
          # Additional actions or warning handling if needed
        }
      )
    }
    
    if(generate_plots == T){
      png(filename = paste0(plot_dir, dyad,"_",trial,"_cleaned_missed_hits.png"), width = 720*scale, height = 480*scale, res = 72*scale)
      print(gg_s(data, title_mod = ": Fully Cleaned"))
      dev.off()    
    }
    
    tryCatch(
      {
        data <- detrend_cont(data)
        data <- desynch_flag(data)
        
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
        data <- desynch_flag(data)
        
      }
    )
    
     
    if("onset_diff_1p_detrend" %in% colnames(data) & generate_plots == T){
      png(filename = paste0(plot_dir, dyad,"_",trial,"_detrend.png"), width = 720*scale, height = 480*scale, res = 72*scale)
      print(gg_s(data, detrend = T))
      dev.off()
    }
    
      data <- mark_excluded_ibis(data)

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
   #write_rds(full_dyad_data, paste0("X:\\Sean M\\Social_Drumming\\beh_sync_output\\", dyad, "_output.rds"))
   write_rds(full_dyad_data, paste0("C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\temp_output\\", dyad, "_output.rds"))
  
}
end <- Sys.time()
elapsed <- print(end-start)
