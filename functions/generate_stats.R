generate_stats <- function(data){
  
  #### TO-DO
  # 1. Split into sync phase, cont phase, all
  # 2. Split for alt condition
  
  p_idx <- which(data$participant == 1)
  mean_async <- data %>% group_by(hit_number_participant) %>%
    mutate(async = start_s[1] - start_s[2]) %>%
    mutate(group_hit = (start_s[1]+start_s[2]) / 2) %>%
    filter(all(imputed == 0))
  
  mean_async <- mean_async[-seq(1, nrow(mean_async), 2),]
  async_hist <- hist(mean_async$async)
  #mean pairwise asynchrony
  mpa <- sum(abs(mean_async$async))/nrow(mean_async)
  
  pairwise_asynch <- sqrt(sum(abs(mean_async$async) - mpa)^2 / (nrow(mean_async)-1))
  
  
  ccf_list <- ccf(data$start_s[p_idx], data$start_s[-p_idx], lag.max = 15)
  n_taps <- nrow(data %>% filter(start_s >= 17))
  time_plot <- gg_s(data)
  
  
  #will differ between conditions
  cont_bpm <- n_taps/2
  n_imputed <- sum(data$imputed)
  toss <- (any(data$onset_diff_1p > 3, na.rm = T))
  asynchs <- psych::describe(mean_async$async)
  output <- list(toss, asynchs, async_hist, pairwise_asynch, mpa, ccf_list, time_plot, cont_bpm, n_imputed)
  names(output) <- c("Exclude Trial", "Describe: Asychronies", "Asynch Histogram", "Precision: Pairwise Asynchrony", "Accuracy: Onset Asynchrony", "Cross-Correlation Function", "Time Series", "Continuation Phase BPM", "N Imputed")
  return(output)
}
