flip_participants <- function(data){
  if(trial %in% c(2,4)){
    data$participant <- ifelse(data$participant == 1, 2, 1)
  }
  return(data)
}
