remove_double_hits <- function(data){
  if(dyad < 200){
    
    data$double_hit_flag <- data$onset_diff_2p < .15 & (data$participant == lag(data$participant, 1))
    data <- data %>% filter(double_hit_flag == FALSE | is.na(double_hit_flag))
    
  } else if(dyad > 200 & dyad < 300){

    data$double_hit_flag <- ifelse(data$onset_diff_1p < .15, 1, 0)
    idx <- which(data$double_hit_flag == 1)
    if(length(idx) > 0){
      data <- data[-idx,]
        
    }
  }
  return(data)
}