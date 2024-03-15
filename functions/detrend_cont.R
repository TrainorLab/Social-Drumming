detrend_cont <- function(data, poly = 1, lowess = FALSE){
  
  if(dyad <200){
    cont_start <- 15.85
  } else if(dyad >200 &  dyad <300){
    cont_start <- 31.85
  }  
  data_start <- data %>% filter(start_s < cont_start)
  data_cont <- data %>% filter(start_s >= cont_start)
  
  
  if(dyad <200){
    
    # Warning message for 110.1 - removed chunk at beginning caused detrend to break
    if(any(is.na(data_cont$onset_diff_1p))){
      print("Warning: Removing NAs present in Onset Diff 1p")
    }
    data_cont <- data_cont %>% 
      ungroup() %>%
      filter(!is.na(onset_diff_1p))
    

    p1_onset_diff <- data_cont %>% filter(participant == 1) %>% select(onset_diff_1p, start_s, participant, hit_number_participant)
    p2_onset_diff <- data_cont %>% filter(participant == 2) %>% select(onset_diff_1p, start_s, participant, hit_number_participant)
    
    mod1 <- lm(data = p1_onset_diff, onset_diff_1p ~ poly(start_s, poly))
    mod2 <- lm(data = p2_onset_diff, onset_diff_1p ~ poly(start_s, poly))
    
    
    p1_onset_diff$onset_diff_1p_detrend <-  mean(p1_onset_diff$onset_diff_1p) + mod1$residuals
    p2_onset_diff$onset_diff_1p_detrend <-  mean(p2_onset_diff$onset_diff_1p) + mod2$residuals
    
    
    
    temp <- bind_rows(p1_onset_diff, p2_onset_diff) 
    
    data_cont <- full_join(data_cont, temp, by = c("hit_number_participant", "onset_diff_1p", "participant", "start_s"))
    
      
    data_cont <- data_cont %>%
      ungroup() %>%
      group_by(participant) %>%
      mutate(start_s_detrend = onset_diff_1p_detrend + lag(start_s)) %>% 
      ungroup() %>%
      mutate(onset_diff_2p_detrend = start_s_detrend - lag(start_s_detrend))
    
    #mod <- lm(data = data_cont, onset_diff_2p ~ poly(start_s, poly))
      # data_cont <- data_cont %>% 
      #   mutate(onset_diff_2p_detrend = mean(onset_diff_2p) + mod$residuals) %>%
      #   mutate(start_s_detrend = onset_diff_2p_detrend + lag(start_s)) %>% 
      #   group_by(participant) %>%
      #   mutate(onset_diff_1p_detrend = start_s_detrend - lag(start_s_detrend))
    
    #data_cont$start_s_detrend[2] <- data_cont$start_s_detrend[3] - data_cont$onset_diff_2p_detrend[3]
    #data_cont$start_s_detrend[1] <- data_cont$start_s_detrend[2] - data_cont$onset_diff_2p_detrend[2]
  
    } else if(dyad >200 &  dyad <300){
    
      
      data_cont <- data_cont %>% 
        group_by(hit_number_participant) %>%
        mutate(group_2p_onset_avg = (onset_diff_2p[1]+onset_diff_2p[2]) / 2)
      
      if(nrow(data_cont) %% 2 == 1){
        data_cont <- data_cont[-nrow(data_cont),]
      }
    
      p1_onset_diff <- data_cont %>% filter(participant == 1) %>% select(onset_diff_1p, start_s, participant, hit_number_participant)
      p2_onset_diff <- data_cont %>% filter(participant == 2) %>% select(onset_diff_1p, start_s, participant, hit_number_participant)
      
      mod1 <- lm(data = p1_onset_diff, onset_diff_1p ~ poly(start_s, poly))
      mod2 <- lm(data = p2_onset_diff, onset_diff_1p ~ poly(start_s, poly))
      
      p1_onset_diff$onset_diff_1p_detrend <-  mean(p1_onset_diff$onset_diff_1p) + mod1$residuals
      p2_onset_diff$onset_diff_1p_detrend <-  mean(p2_onset_diff$onset_diff_1p) + mod2$residuals
      
      temp <- bind_rows(p1_onset_diff, p2_onset_diff) 
      
      data_cont <- full_join(data_cont, temp, by = c("hit_number_participant", "participant", "onset_diff_1p", "start_s"))
      
      
    data_cont <- data_cont %>%
      ungroup() %>%
      group_by(participant) %>%
      mutate(start_s_detrend = onset_diff_1p_detrend + lag(start_s)) %>% 
      ungroup() %>%
      mutate(onset_diff_2p_detrend = start_s_detrend - lag(start_s_detrend))
      
      # ggplot(data = data_cont, aes(x = start_s, y = onset_diff_1p, color = as.factor(participant))) +
      #   geom_point() +
      #   geom_line() +
      #   theme_bw()
      # 
      # ggplot(data = data_cont, aes(x = start_s_detrend, y = onset_diff_1p_detrend, color = as.factor(participant))) +
      #   geom_point() +
      #   geom_line() +
      #   theme_bw()
      # 
      # 
  }  
  
  
  data <- bind_rows(data_start, data_cont) 

  return(data)
}
