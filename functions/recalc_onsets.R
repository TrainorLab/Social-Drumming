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
  data$double_skip_flag <- ifelse(data$onset_diff_1p > 2.5 & data$onset_diff_1p < 3.5, 1, 0)
  
  return(data)
}
