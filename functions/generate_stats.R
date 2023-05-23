generate_stats <- function(data){
  
  
  #### TO-DO
  # 1. Split into sync phase, cont phase, all
  # 2. Split for alt condition
  # 3. Resolve weird async issues like 203-3
  
  
  p_idx <- which(data$participant == 1)
  mean_async <- data %>% group_by(hit_number_participant) %>%
    mutate(async = start_s[1] - start_s[2]) %>%
    mutate(group_hit = (start_s[1]+start_s[2]) / 2) %>%
    filter(all(imputed == 0))
  
  mean_async <- mean_async[-seq(1, nrow(mean_async), 2),]
  hist(mean_async$async)
  #mean pairwise asynchrony
  mpa <- sum(abs(mean_async$async))/nrow(mean_async)
  
  pairwise_asynch <- sqrt(sum(abs(mean_async$async) - mpa)^2 / (nrow(mean_async)-1))
  
  
  ccf_list <- ccf(data$start_s[p_idx], data$start_s[-p_idx])
  lag0_corr <- ccf_list[["acf"]][[17]]
  
  n_taps <- nrow(data %>% filter(start_s >= 17))
  
  #will differ between conditions
  cont_bpm <- n_taps/2
  toss <- (any(data$onset_diff_1p > 3, na.rm = T))
  asynchs <- psych::describe(mean_async$async)
  output <- list(toss, asynchs, pairwise_asynch, mpa, lag0_corr, cont_bpm)
  names(output) <- c("Exclude Trial", "Describe: Asychronies", "Precision: Pairwise Asynchrony", "Accuracy: Onset Asynchrony", "Lag 0 Cross-Correlation", "Continuation Phase BPM")
  return(output)
}
