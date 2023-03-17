### Social Drumming Analysis ###
library(tidyverse)
library(zoo)
library(ggplot2)
rm(list=ls())

###TASK###
# 3Put expected pulse for synch condition from kick and snare and deviation
# 2accuracy metric
# 1group by even and odd trial such that participant ID consistent across trials
# 4 use midi as the model of how we want to visualize

# Plotting boundaries of end of synch phase and when they drum independent

# Importing
setwd("~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation/Data")


#101_trial 1 bad


data <- read.csv("101_trial1.csv",stringsAsFactors = T)

# Data Cleaning and Restructuring

# Renaming variables to be readable
data <- data %>%
  mutate(s_ppq = (s_ppq/(960*2)),e_ppq = (e_ppq/(960*2))) %>%
  rename(start_s = s_ppq,end_s = e_ppq) %>%
  select(-sel, -mut, -chan, -vel)


#adding a participant number to each drummer
data$participant <- ifelse(data$pitch==47,1,2)

data$participant <- as.factor(data$participant)


#onset difference between 2 participants and individual participants
data$onset_diff_2p <- data$start_s - lag(data$start_s, 1)

data <- data %>% 
  group_by(participant) %>%
  mutate(onset_diff_1p = start_s - lag(start_s, 1))


#Skip flagging mechanisms
data$flag_skip <- ifelse(data$participant + lag(data$participant,1) == 3,0,1)


# Rolling averages
data <- data %>% 
  group_by(participant) %>%
  mutate(roll_1p = rollmean(onset_diff_1p, k=5, align = "right", fill = NA))

data <- data %>% 
  ungroup %>%
  mutate(roll_2p = rollmean(onset_diff_2p, k=5, align = "right", fill = NA))


# Cleaning pt 2
data <- data %>%
  select(-start_s, -end_s, -leng, -pitch)

# Imputing missed hits
data_cut <- data[1:which(data$flag_skip==1)[1] - 1,]


new_row <- tibble(participant=1,
                  onset_diff_2p=data$roll_2p[61],



data_cut <- data_cut %>%
  rows_insert(new_row, by = names(new_row))


which(data$flag_skip==1)
gap <- (which(data$flag_skip==1)[2])-(which(data$flag_skip==1)[1])

data_cut2 <- data[which(data$flag_skip==1)[1]+1:gap-1,]

new_row2 <- tibble(participant=1,
                  onset_diff_2p=data$roll_2p[9],


data_cut3 <- data[which(data$flag_skip==1)[2] + 1:41,]

data_list <- list(data_cut, data_cut2, data_cut3)


# Flags for speed
flag_2p <- 2*sd(data$onset_diff_2p, na.rm = T)
data$flag_2p_fast <- ifelse(data$onset_diff_2p < mean(data$onset_diff_2p,na.rm=T)-flag_2p,1,0)
data$flag_2p_slow <- ifelse(data$onset_diff_2p > mean(data$onset_diff_2p,na.rm=T)+flag_2p,1,0)

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




ggplot(data=data_fixed, aes(x=hit_number, y=onset_diff_2p)) +
  geom_line(aes(group=participant, color=participant))+
  geom_point(aes(group=participant, color=participant))+
  geom_smooth(data=data_fixed, aes(x=hit_number,y = roll_2p),size=2)+
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

