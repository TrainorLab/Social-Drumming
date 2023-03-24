load_data <- function(dyad, trial){
  data <- read.csv(paste0(data_dir, dyad, "_trial", trial, ".csv"),stringsAsFactors = T)
  names(data) <- c("sel", "mut", "s_ppq",  "e_ppq", "leng", "chan", "pitch", "vel")  
  
  data <- data %>%
    mutate(s_ppq = (s_ppq/(960)),e_ppq = (e_ppq/(960))) %>%
    rename(start_s = s_ppq,end_s = e_ppq) %>%
    select(-sel, -mut, -chan, -vel)
  
  # Data Cleaning and Restructuring
  
  # Renaming variables to be readable
  # FOR SYNC: NOT 960*2, only 960 (sampling rate)
  
  
  #adding a participant number to each drummer
  data$participant <- ifelse(data$pitch==47,1,2)
  
  
  #onset difference between 2 participants and individual participants
  data$onset_diff_2p <- data$start_s - lag(data$start_s, 1)
  
  data <- data %>% 
    group_by(participant) %>%
    mutate(onset_diff_1p = start_s - lag(start_s, 1))
  
  
  
  #Skip flagging mechanisms
  data$skip_flag <- ifelse(data$onset_diff_1p > 1.5 & data$onset_diff_1p < 2.5, 1, 0)
  data$double_skip_flag <- ifelse(data$onset_diff_1p > 2.5 & data$onset_diff_1p < 3.5, 1, 0)
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
    mutate(hit_number_participant = seq_along(participant)) %>%
    mutate(imputed = 0)
}
