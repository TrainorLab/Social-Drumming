generate_stats <- function(data){
  
  p_idx <- which(data$participant == 1)
  mean_async <- data %>% group_by(hit_number_participant) %>%
    mutate(async = start_s[1] - start_s[2]) %>%
    filter(all(imputed == 0))
  
  mean_async <- mean_async[-seq(1, nrow(mean_async), 2),]
  hist(mean_async$async)
  moa <- mean(abs(mean_async$async))
  
  ccf_list <- ccf(data$start_s[p_idx], data$start_s[-p_idx])
  lag0_corr <- ccf_list[["acf"]][[17]]
  
  n_taps <- nrow(data %>% filter(start_s >= 17))
  
  #will differ between conditions
  cont_bpm <- n_taps/2
  
  output <- list(moa, lag0_corr, cont_bpm)
  names(output) <- c("Onset Asynchrony", "Lag 0 Cross-Correlation", "Continuation Phase BPM")
  return(output)
}
