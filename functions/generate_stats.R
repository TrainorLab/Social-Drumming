generate_stats <- function(data){
  
  if(dyad <200){
    cont_start <- 16
    condition_b <- 1
  } else if(dyad >200 &  dyad <300){
    cont_start <- 32
    condition_b <- 2
  }
  
  data <- data %>% 
    group_by(hit_number_participant) %>%
    mutate(exclude_IBI = any(imputed == 1)) %>%
    ungroup() %>%
    mutate(exclude_IBI2 = dplyr::lag(imputed) == 1 | dplyr::lag(imputed, 2) == 1) %>%
    group_by(hit_number_participant) %>%
    mutate(exclude_IBI3 = any(exclude_IBI2) == T) %>%
    mutate(exclude_IBI = exclude_IBI3) %>%
    select(-exclude_IBI2, -exclude_IBI3)
           
  # First, we need to calculate the onset asynchronies, which is just the difference
  # in seconds between the first and second hit for each event. We also calculate the 
  # group/average onset time
  async <- data %>% group_by(hit_number_participant) %>%
    mutate(async = start_s[1] - start_s[2]) %>%
    mutate(group_hit = (start_s[1]+start_s[2]) / 2) %>%
    filter(all(imputed == 0)) %>%
    mutate(async_detrend = start_s_detrend[1] - start_s_detrend[2])
  
  #now we need to remove ever other row, as they are redundant, as we've calculated 
  # the difference between the two rows
  async <- async[-seq(1, nrow(async), 2),]
  # plot their distribution and autocorrelation, Repp & Keller, 2008
  # AC1 should be positive
  async_sync_phase <- async %>% filter(start_s < cont_start)
  async_cont_phase <- async %>% filter(start_s >= cont_start)
  
  async_sync_hist <- hist(async_sync_phase$async, plot = FALSE)
  async_cont_hist <- hist(async_cont_phase$async, plot = FALSE)
  
  async_sync_acf <- acf(async_sync_phase$async, na.action = na.pass, plot = FALSE)
  async_cont_acf <- acf(async_cont_phase$async, na.action = na.pass, plot = FALSE)
  
  asyncs_sync <- psych::describe(async_sync_phase$async)
  asyncs_cont <- psych::describe(async_cont_phase$async)
  
  
  
  # The ITI's of each participant will also have their own time-series and ACF. 
  # AC1 should be negative here 
  
  ITI_2p <- data %>%
    group_by(hit_number_participant) %>%
    mutate(group_hit = (start_s[1]+start_s[2]) / 2) %>%
    mutate(detrend_group_hit = (start_s_detrend[1]+start_s_detrend[2]) / 2 ) %>%
    ungroup() %>%
    mutate(group_IBI = group_hit - lag(group_hit)) %>%
    mutate(detrend_group_IBI = detrend_group_hit - lag(detrend_group_hit)) %>%
    group_by(hit_number_participant) 
  
  
  ITI_2p <- ITI_2p[seq(1, nrow(ITI_2p), 2),]    
  
  ITI_2p_sync <- ITI_2p %>%
    filter(exclude_IBI == FALSE) %>%
    filter(start_s < cont_start)
  
  ITI_2p_cont <- ITI_2p %>%
    filter(exclude_IBI == FALSE) %>%
    filter(start_s >= cont_start)

   
  ITI_1_sync <- data %>% 
    group_by(hit_number_participant) %>%
    filter(exclude_IBI == FALSE) %>%
    #filter(all(imputed == 0)) %>%
    filter(!is.na(onset_diff_1p) & participant == 1) %>%
    filter(start_s < cont_start)
  
  ITI_1_cont <- data %>% 
    group_by(hit_number_participant) %>%
    filter(exclude_IBI == FALSE) %>%
    #filter(all(imputed == 0)) %>%
    filter(!is.na(onset_diff_1p) & participant == 1) %>%
    filter(start_s >= cont_start)
  
  ITI_2_sync <- data %>% 
    group_by(hit_number_participant) %>%
    filter(exclude_IBI == FALSE) %>%
    #filter(all(imputed == 0)) %>%
    filter(!is.na(onset_diff_1p) & participant == 2) %>%
    filter(start_s < cont_start)
  
  ITI_2_cont <- data %>% 
    group_by(hit_number_participant) %>%
    filter(exclude_IBI == FALSE) %>%
    #filter(all(imputed == 0)) %>%
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
  
  #1p IBI variability person
  p1_IBI_var_sync <- var(ITI_1_sync$onset_diff_1p)
  p2_IBI_var_sync <- var(ITI_2_sync$onset_diff_1p)
  
  p1_IBI_var_cont <- var(ITI_1_cont$onset_diff_1p)
  p2_IBI_var_cont <- var(ITI_2_cont$onset_diff_1p)
  
  #1p ITI variability - detrended
  p1_detrend_IBI_var_cont <- var(ITI_1_cont$onset_diff_1p_detrend, na.rm  = TRUE )
  p2_detrend_IBI_var_cont <- var(ITI_2_cont$onset_diff_1p_detrend, na.rm  = TRUE)
  
  #2p IBI variability
  if(dyad > 200){
    IBI_2p_var_sync <- var(ITI_2p_sync$group_IBI - lag(ITI_2p_sync$group_IBI), na.rm = TRUE)
    IBI_2p_var_cont <- var(ITI_2p_cont$group_IBI - lag(ITI_2p_cont$group_IBI), na.rm = TRUE)
    IBI_2p_detrend_var_cont <- var(ITI_2p_cont$detrend_group_IBI - lag(ITI_2p_cont$detrend_group_IBI), na.rm = TRUE)
  } else if (dyad < 200){
    IBI_2p_var_sync <- var(ITI_2p_sync$onset_diff_2p, na.rm = TRUE)
    IBI_2p_var_cont <- var(ITI_2p_cont$onset_diff_2p, na.rm = TRUE)
    IBI_2p_detrend_var_cont <- var(ITI_2p_cont$onset_diff_2p_detrend, na.rm = TRUE)
  }
  
  
  ####
  # TRYcATCH needed in case the detrend function doesn't create the correct variables
  tryCatch(
    {
      p1_ITI_detrended <- acf(ITI_1_cont$onset_diff_1p_detrend, na.action = na.pass, plot = FALSE)
      p2_ITI_detrended <- acf(ITI_2_cont$onset_diff_1p_detrend, na.action = na.pass, plot = FALSE)
      async_detrend_acf <- acf(async_cont_phase$async_detrend, na.action = na.pass, plot = FALSE)
      detrended_plot <- gg_s(data, detrend = T)
    },
    error = function(e) {
      message("An error occurred processing the detrended analyses: ", conditionMessage(e))
      p1_ITI_detrended <- NULL
      p2_ITI_detrended <- NULL
      async_detrend_acf <- NULL
      detrended_plot <- NULL
    },
    warning = function(w) {
      # Code to handle warnings if required
      message("A warning occurred: ", conditionMessage(w))
      # Additional actions or warning handling if needed
      data <- detrend_cont(data)
    }
  )    
  
  
  
  
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
  
  clean_hits <- data %>% 
    group_by(hit_number_participant) %>%
    summarise(
      clean_hits = sum(all(imputed == 0))
    )
  clean_hits2 <- data %>%
    group_by(hit_number_participant) %>%
    summarise(clean_hits2 = sum(all(exclude_IBI == FALSE)))
  clean <- sum(clean_hits$clean_hits)
  clean_pct <- clean/nrow(clean_hits)
  clean2 <- sum(clean_hits2$clean_hits2, na.rm = T)
  clean2_pct <- clean2/nrow(clean_hits2)
  
  
  output <- list(toss, asyncs_sync, asyncs_cont,
                 async_sync_hist, async_cont_hist, 
                 pairwise_async_sync, pairwise_async_cont,
                 mpa_sync, mpa_cont,
                 async_sync_acf, async_cont_acf, 
                 p1_ITI_sync_acf, p1_ITI_cont_acf,
                 p2_ITI_sync_acf, p2_ITI_cont_acf,
                 p1_ITI_detrended, p2_ITI_detrended, async_detrend_acf,
                 p1_IBI_var_sync, p1_IBI_var_cont, p2_IBI_var_sync, p2_IBI_var_cont,
                 p1_detrend_IBI_var_cont, p2_detrend_IBI_var_cont,
                 IBI_2p_var_sync, IBI_2p_var_cont, IBI_2p_detrend_var_cont,
                 onsets_plot, detrended_plot,
                 cont_bpm, n_imputed, clean, clean_pct, 
                 clean2, clean2_pct, raw)
  names(output) <- c("Exclude Trial", "Asychronies: Synchronization Phase", "Asychronies: Continuation Phase",
                     "Async Histogram: Synchronization Phase", "Async Histogram: Continuation Phase",
                     "Precision: Pairwise Asynchrony - Synchronization Phase", "Precision: Pairwise Asynchrony - Continuation Phase",   
                     "Accuracy: Onset Asynchrony - Synchronization Phase", "Accuracy: Onset Asynchrony - Continuation Phase",
                     "Async ACF: Synchronization Phase", "Async ACF: Continuation Phase", 
                     "Participant A: ITI ACF - Synchronization Phase", "Participant A: ITI ACF - Continuation Phase", 
                     "Participant B: ITI ACF - Synchronization Phase", "Participant B: ITI ACF - Continuation Phase", 
                     "Participant A: ITI ACF - Detrended (Cont. Phase)", "Participant B: ITI ACF - Detrended (Cont. Phase)", "Detrended Async ACF: Continuation Phase",
                     "Participant A: Tap Variability - Synchronization Phase", "Participant A: Tap Variability - Continuation Phase", "Participant B: Tap Variability - Synchronization Phase", "Participant B: Tap Variability - Continuation Phase",
                     "Participant A: Detrended Tap Variability - Continuation Phase", "Participant B: Detrended Tap Variability - Continuation Phase",
                     "Dyadic Tap Variability - Synchronization Phase", "Dyadic Tap Variability - Continuation Phase", "Dyadic Tap Variability - Detrended Continuation Phase",
                     "Raw Time Series", "Detrended Time Series",
                     "Continuation Phase BPM", "N Imputed", "Clean Hits", "Percent Clean",
                     "Clean Hits (Following removed)", "Percent Clean (Following removed)",
                     "Raw Data")
  return(output)
}
