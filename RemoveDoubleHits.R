RemoveDoubleHits <- function(data){
  data$double_hit_flag <- data$onset_diff_2p < .1 & (data$participant == lag(data$participant, 1))
   data <- data %>% filter(double_hit_flag == FALSE)  
}
