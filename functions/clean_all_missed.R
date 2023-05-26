clean_all_missed <- function(data, threshold = .6){  
  
  idx_og <- which(data$onset_diff_1p > (1 + threshold))
  
  for(i in 1:length(idx_og)){
    idx <- which(data$onset_diff_1p > 1 + threshold)
    n_missed <- round(data$onset_diff_1p[idx][1]) - 1
    first_missed <- (which(data$onset_diff_1p > n_missed + threshold) - n_missed)[1]
    if(data$onset_diff_2p[first_missed] > .8 && data$onset_diff_2p[first_missed-1] >.8 ){
      first_missed <- first_missed - 1  
    }
    data <- add_synchronous_missed_hits(data = data, n_missed = n_missed, where = first_missed, participant = 3 - data$participant[first_missed])
    data <- recalc_onsets(data)
  }
  return(data)
}
