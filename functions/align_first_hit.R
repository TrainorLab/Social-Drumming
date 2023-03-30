align_first_hit <- function(data){
  if (dyad < 200 ) {
    idx <- (which(data$start_s - lag(data$start_s) < .65)) #& 
      #(which((data$participant + lag(data$participant)==3)) -1)
    data <- data[idx[1]:nrow(data),]
  } else {
    idx <- which(data$start_s - lag(data$start_s) < .15) - 1
    data <- data[idx[1]:nrow(data),]
  }

}

