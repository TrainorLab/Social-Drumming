generate_stats <- function(data, phase = "all"){
  
  if(dyad <200){
    cont_start <- 16
    condition_b <- 1
  } else if(dyad >200 &  dyad <300){
    cont_start <- 32
    condition_b <- 2
  }
  
  if(phase == "cont"){
    data <- data %>% filter(start_s >= cont_start)
  } else if(phase == "sync"){
    data <- data %>% filter(start_s < cont_start)
  } else {}
  
  ### TO-DO
  # 2. Split for alt or sync condition
  
  # First, we need to calculate the onset asynchronies, which is just the difference
  # in seconds between the first and second hit for each event. We also calculate the 
  # group/average onset time
  async <- data %>% group_by(hit_number_participant) %>%
    mutate(async = start_s[1] - start_s[2]) %>%
    mutate(group_hit = (start_s[1]+start_s[2]) / 2) %>%
    filter(all(imputed == 0))
  
  #now we need to remove ever other row, as they are redundant, as we've calculated 
  # the difference between the two rows
  async <- async[-seq(1, nrow(async), 2),]
  # plot their distribution and autocorrelation, Repp & Keller, 2008
  # AC1 should be positive
  async_hist <- hist(async$async)
  async_acf <- acf(async$async, na.action = na.pass)
  asyncs <- psych::describe(async$async)

  # The ITI's of each participant will also have their own time-series and ACF. 
  # AC1 should be negative here 
   
  ITI_1 <- data %>% 
    filter(!is.na(onset_diff_1p) & participant == 1) %>%
    filter(all(imputed == 0))
  ITI_2 <- data %>% 
    filter(!is.na(onset_diff_1p) & participant == 2) %>%
    filter(all(imputed == 0))
  
  p1_ITI_acf <- acf(ITI_1$onset_diff_1p)
  p2_ITI_acf <- acf(ITI_2$onset_diff_1p)
  
  
  #mean pairwise asynchrony (from onsetsync package documentation)
  mpa <- sum(abs(async$async))/nrow(async)
  pairwise_async <- sqrt(sum(abs(async$async) - mpa)^2 / (nrow(async)-1))
  
  # We'll also calculate and plot the CCF of the onset-times of each tapper 
  p_idx <- which(data$participant == 1)
  ccf_list <- ccf(data$start_s[p_idx], data$start_s[-p_idx], lag.max = 15, na.action = na.pass)
  onsets_plot <- gg_s(data)
  
  # will differ between conditions, thus we use cont_start and condition_b
  n_taps <- nrow(data %>% filter(start_s >= cont_start))
  
  cont_bpm <- n_taps/condition_b
  n_imputed <- sum(data$imputed)
  toss <- (any(data$onset_diff_1p > 3, na.rm = T))
  raw <- data
  
  output <- list(toss, asyncs, async_hist, pairwise_async, mpa, ccf_list, async_acf, p1_ITI_acf, p2_ITI_acf, onsets_plot, cont_bpm, n_imputed, raw)
  names(output) <- c("Exclude Trial", "Describe: Asychronies", "Async Histogram", "Precision: Pairwise Asynchrony", "Accuracy: Onset Asynchrony", "Cross-Correlation Function", "Async ACF", "Participant A: ITI ACF", "Participant B: ITI ACF", "Raw Time Series", "Continuation Phase BPM", "N Imputed", "Raw Data")
  return(output)
}