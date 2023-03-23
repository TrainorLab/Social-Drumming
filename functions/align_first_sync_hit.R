align_first_sync_hit <- function(data){
  idx <- which(data$start_s - lag(data$start_s) < .1) - 1
  data <- data[idx[1]:nrow(data),]
}
