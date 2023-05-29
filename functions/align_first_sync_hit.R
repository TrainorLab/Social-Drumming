align_first_sync_hit <- function(data){
  
  if(dyad <200){
    idx <- which(data$start_s - lag(data$start_s) < .9) - 1
    data <- data[idx[1]:nrow(data),]
    
  } else if(dyad >200 &  dyad <300){
    idx <- which(data$start_s - lag(data$start_s) < .15) - 1
    data <- data[idx[1]:nrow(data),]
  }
  
  return(data)
}
