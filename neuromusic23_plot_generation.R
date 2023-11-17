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
dyad <- 217
trial <- 2

data <- load_data(dyad, trial)
data <- flip_participants(data)
data <- remove_double_hits(data)
data <- align_first_hit(data)
data <- trim_end(data)
data <- modify_individual_trial(data)
data <- recalc_onsets(data)


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







data$participant <- as.factor(data$participant)
data$hit_number <- 1:nrow(data)











if(dyad <200){
  synch_phase_end <- 16
} else if(dyad >200 &  dyad <300){
  synch_phase_end <- 32
}




gg_s(data)





scale = 3.5
png("C:\\Users\\mcwee\\Documents\\LIVELab\\NeuroMusic23\\plots\\individual_data_plot.png", width = 1200*scale, height = 480*scale, res = 72*scale)
ggplot(data=data, aes(x=start_s, y=onset_diff_1p, group=participant)) +
  geom_line(aes(color=participant))+
  geom_point(aes(color=participant))+
  geom_vline(xintercept = synch_phase_end, size=1)+
  labs(title = paste0(dyad, " Trial #",  trial), x = "Onset Time (s)", y = "Inter-Tap Interval (Per Person)",
       color = "Participant") +
  theme_bw() +
  ggplot2::theme(axis.title = element_text(size = 20), 
                 axis.text = element_text(size = 18),
                 legend.title = element_text(size = 16), 
                 legend.text = element_text(size = 14),
                 title = element_text(size = 24)) 

dev.off()


png("C:\\Users\\mcwee\\Documents\\LIVELab\\NeuroMusic23\\plots\\individual_data_detrend_plot.png", width = 1200*scale, height = 480*scale, res = 72*scale)
ggplot(data=data, aes(x=start_s_detrend, y=onset_diff_1p_detrend, group=participant)) +
  geom_line(aes(color=participant))+
  geom_point(aes(color=participant))+
  geom_vline(xintercept = synch_phase_end, size=1)+
  labs(title = paste0(dyad, " Trial #",  trial, ": Detrended"), x = "Onset Time (s)", y = "Inter-Tap Interval (Per Person)",
       color = "Participant") +
  theme_bw() +
  ggplot2::theme(axis.title = element_text(size = 20), 
                 axis.text = element_text(size = 18),
                 legend.title = element_text(size = 16), 
                 legend.text = element_text(size = 14),
                 title = element_text(size = 24)) 

dev.off()




