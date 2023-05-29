align_first_hit <- function(data){
  if (dyad < 200 ) {
    data <- data[data$start_s > 4,]
    
    
    idx <- (which(data$start_s - lag(data$start_s) < .8 | is.na(data$onset_diff_2p)))
    data <- data[idx[1]:nrow(data),]
    idx2 <- which(data$participant - lag(data$participant) == 0)
    
    if(!is_empty(idx2) && data$start_s[idx2[length(idx2)]] < 16){
      data <- data[idx2[length(idx2)]:nrow(data),]
    } else if(!is_empty(idx2) && data$start_s[length(idx2)] > 16){
        data <- NULL
    } else {
      data <- data
    }
    
  } else if(dyad >200 &  dyad <300){
    idx <- which(data$start_s - lag(data$start_s) < .15) - 1
    data <- data[idx[1]:nrow(data),]
  }
  return(data)
}

