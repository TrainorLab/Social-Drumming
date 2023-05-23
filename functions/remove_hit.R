remove_hit <- function(data, participant, hit){
  data <- data[-which(data$participant == participant & data$hit_number_participant == hit),] 
  data <- recalc_onsets(data)
}