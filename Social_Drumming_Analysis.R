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

source("InsertMissedHits.R")
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
dyad <- 104
trial <- 2

data <- load_data(dyad, trial)

gg_s(data)

data <- remove_double_hits(data)

which(data$skip_flag == 1)
sum(data$skip_flag, na.rm = T)


data_full <- InsertMissedHits(data)

gg_s(data_full)

##Remaining Issues: multiple missed hits in a row
## missed hit before a rolling avg is calculated
## Recalculate rolling avg?
## skip flagging mechanism catching double hits: see 108_3, 109_1



#Recalculate rolling avgs

######


  
# Flags for speed
flag_2p <- 2*sd(data$onset_diff_2p, na.rm = T)
data$flag_2p_fast <- ifelse(data$onset_diff_2p < mean(data$onset_diff_2p,na.rm=T)-flag_2p,1,0)
data$flag_2p_slow <- ifelse(data$onset_diff_2p > mean(data$onset_diff_2p,na.rm=T)+flag_2p,1,0)

#flag_1p <- 2*sd(data$onset_diff_1p, na.rm = T)
#data$flag_1p_fast <- ifelse(data$onset_diff_1p < mean(data$onset_diff_1p,na.rm=T)-flag_1p,1,0)
#data$flag_1p_slow <- ifelse(data$onset_diff_1p > mean(data$onset_diff_1p,na.rm=T)+flag_1p,1,0)


# imputing mechanism
# data$onset_diff_1p_est <- ifelse(data$flag_1p==1,(data$roll_1p + lag(data$onset_diff_1p,1)),data$onset_diff_1p)

data$onset_diff_2p_est1 <- ifelse(data$flag_2p_fast==1,(data$roll_2p + lag(data$onset_diff_2p,1)),data$onset_diff_2p)
data$onset_diff_2p_est <- ifelse(data$flag_2p_slow==1,(data$roll_2p + lag(data$onset_diff_2p_est1,1)),data$onset_diff_2p_est1)


# Basic line plot with points
data$participant <- as.factor(data$participant)
data$hit_number <- 1:nrow(data) 

ggplot(data=data, aes(x=hit_number, y=onset_diff_1p_est, group=participant)) +
  geom_line(aes(color=participant))+
  geom_point(aes(color=participant))+
  geom_smooth(data=data_fixed, aes(x=hit_number, y=roll_1p, color=participant),size=2)+
  geom_vline(xintercept = 15,size=1)


ggplot(data=data, aes(x=hit_number, y=onset_diff_2p_est)) +
  geom_line(aes(group=participant, color=participant))+
  geom_point(aes(group=participant, color=participant))+
  geom_smooth(data=data, aes(x=hit_number,y = roll_2p),size=2)+
  geom_vline(xintercept = 15,size=1)





# Pulling data, identifying condition, creating total dataframe ####

# collecting alternating condition from set of files
data_files = list.files(path = ".", pattern="10") #add|11|12 and so on with more data"
alt_data   = list()

# adding all files to a list
for (ii in 1:length(data_files)){
  temp = read.csv(data_files[ii])
    alt_data[[ii]] = temp
  }

# creating dataframe from the list
alt_data = alt_data[lapply(alt_data,length)>0]
alt_data = bind_rows(alt_data)
alt_data$condition <- 1


# collecting synch condition from set of files
data_files = list.files(path = ".", pattern="20")
synch_data   = list()

# adding all files to a list
for (ii in 1:length(data_files)){
  temp = read.csv(data_files[ii])
  synch_data[[ii]] = temp
}

# creating dataframe from the list
synch_data = synch_data[lapply(synch_data,length)>0]
synch_data = bind_rows(synch_data)
synch_data$condition <- 2

# collecting alone condition from set of files
data_files = list.files(path = ".", pattern="30")
alone_data   = list()

# adding all files to a list
for (ii in 1:length(data_files)){
  temp = read.csv(data_files[ii])
  alone_data[[ii]] = temp
}

# creating dataframe from the list
alone_data = alone_data[lapply(alone_data,length)>0]
alone_data = bind_rows(alone_data)
alone_data$condition <- 3

#adding all the separate dataframes
all_data <- rbind(alt_data,synch_data,alone_data)


# row_num <- which(data$flag_skip==1)
# newrow <- tibble(participant=1,
#                  onset_diff_2p=data$roll_2p[61],
#                  onset_diff_1p=data$roll_1p[60])
# 
# for (i in row_num){
#   newrow <- tibble(participant=data$participant[i-1],
#                    )
#   insertRow(data,newrow,i)
# }