generate_stats <- function(data){
  
  if(dyad <200){
    cont_start <- 16
    condition_b <- 1
  } else if(dyad >200 &  dyad <300){
    cont_start <- 32
    condition_b <- 2
  }
  
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
  async_sync_phase <- async %>% filter(start_s < cont_start)
  async_cont_phase <- async %>% filter(start_s >= cont_start)
  
  async_sync_hist <- hist(async_sync_phase$async)
  async_cont_hist <- hist(async_cont_phase$async)
  
  async_sync_acf <- acf(async_sync_phase$async, na.action = na.pass, plot = FALSE)
  async_cont_acf <- acf(async_cont_phase$async, na.action = na.pass, plot = FALSE)
  
  asyncs_sync <- psych::describe(async_sync_phase$async)
  asyncs_cont <- psych::describe(async_cont_phase$async)
  
  # The ITI's of each participant will also have their own time-series and ACF. 
  # AC1 should be negative here 
   
  ITI_1_sync <- data %>% 
    group_by(hit_number_participant) %>%
    filter(all(imputed == 0)) %>%
    filter(!is.na(onset_diff_1p) & participant == 1) %>%
    filter(start_s < cont_start)
  
  ITI_1_cont <- data %>% 
    group_by(hit_number_participant) %>%
    filter(all(imputed == 0)) %>%
    filter(!is.na(onset_diff_1p) & participant == 1) %>%
    filter(start_s >= cont_start)
  
  ITI_2_sync <- data %>% 
    group_by(hit_number_participant) %>%
    filter(all(imputed == 0)) %>%
    filter(!is.na(onset_diff_1p) & participant == 2) %>%
    filter(start_s < cont_start)
  
  ITI_2_cont <- data %>% 
    group_by(hit_number_participant) %>%
    filter(all(imputed == 0)) %>%
    filter(!is.na(onset_diff_1p) & participant == 2) %>%
    filter(start_s >= cont_start)
  
  p1_ITI_sync_acf <- acf(ITI_1_sync$onset_diff_1p, plot = FALSE)
  p1_ITI_cont_acf <- acf(ITI_1_cont$onset_diff_1p, plot = FALSE)
  
  p2_ITI_sync_acf <- acf(ITI_2_sync$onset_diff_1p, plot = FALSE)
  p2_ITI_cont_acf <- acf(ITI_2_cont$onset_diff_1p, plot = FALSE)
  
  
  #mean pairwise asynchrony (from onsetsync package documentation)
  mpa_sync <- sum(abs(async_sync_phase$async))/nrow(async_sync_phase)
  mpa_cont <- sum(abs(async_cont_phase$async))/nrow(async_cont_phase)
  
  pairwise_async_sync <- sqrt(sum(abs(async_sync_phase$async) - mpa_sync)^2 / (nrow(async_sync_phase)-1))
  pairwise_async_cont <- sqrt(sum(abs(async_cont_phase$async) - mpa_cont)^2 / (nrow(async_cont_phase)-1))
  
  
  # We'll also calculate and plot the CCF of the onset-times of each tapper 
  #p_idx <- which(data$participant == 1)
  #ccf_list <- ccf(data$start_s[p_idx], data$start_s[-p_idx], lag.max = 15, na.action = na.pass)
  onsets_plot <- gg_s(data)
  
  # will differ between conditions, thus we use cont_start and condition_b
  n_taps <- nrow(data %>% filter(start_s >= cont_start))
  cont_bpm <- n_taps/condition_b
  if(dyad %in% c(101:105, 118:119)){
    cont_bpm <- round(cont_bpm*4/3, 2)
  }
  
  n_imputed <- sum(data$imputed)
  toss <- (any(data$onset_diff_1p > 3, na.rm = T))
  raw <- data
  
  output <- list(toss, asyncs_sync, asyncs_cont,
                 async_sync_hist, async_cont_hist, 
                 pairwise_async_sync, pairwise_async_cont,
                 mpa_sync, mpa_cont,
                 async_sync_acf, async_cont_acf, 
                 p1_ITI_sync_acf, p1_ITI_cont_acf,
                 p2_ITI_sync_acf, p2_ITI_cont_acf,
                 onsets_plot, cont_bpm, n_imputed, raw)
  names(output) <- c("Exclude Trial", "Asychronies: Synchronization Phase", "Asychronies: Continuation Phase",
                     "Async Histogram: Synchronization Phase", "Async Histogram: Continuation Phase",
                     "Precision: Pairwise Asynchrony - Synchronization Phase", "Precision: Pairwise Asynchrony - Continuation Phase",   
                     "Accuracy: Onset Asynchrony - Synchronization Phase", "Accuracy: Onset Asynchrony - Continuation Phase",
                     "Async ACF: Synchronization Phase", "Async ACF: Continuation Phase", 
                     "Participant A: ITI ACF - Synchronization Phase", "Participant A: ITI ACF - Continuation Phase", 
                     "Participant B: ITI ACF - Synchronization Phase", "Participant B: ITI ACF - Continuation Phase", 
                     "Raw Time Series", "Continuation Phase BPM", "N Imputed", "Raw Data")
  return(output)
}