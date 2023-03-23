remove_double_hits <- function(data){
  data$double_hit_flag <- ifelse(data$onset_diff_1p < .1, 1, 0)
  idx <- which(data$double_hit_flag == 1)
  if(length(idx) > 0){
    data <- data[-idx,]
  }
  return(data)
}
