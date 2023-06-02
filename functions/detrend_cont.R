detrend_cont <- function(data, poly = 1){
  
  if(dyad <200){
    cont_start <- 15.85
  } else if(dyad >200 &  dyad <300){
    cont_start <- 31.85
  }  
  data_start <- data %>% filter(start_s < cont_start)
  data_cont <- data %>% filter(start_s >= cont_start)
  
  
  if(dyad <200){
    data_cont <- data_cont %>% 
      mutate(onset_diff_2p_detrend = astsa::detrend(onset_diff_2p, order = poly) + onset_diff_2p) %>%
      mutate(start_s_detrend = onset_diff_2p_detrend + lag(start_s)) %>% 
      group_by(participant) %>%
      mutate(onset_diff_1p_detrend = start_s_detrend - lag(start_s_detrend))
    
    #data_cont$start_s_detrend[2] <- data_cont$start_s_detrend[3] - data_cont$onset_diff_2p_detrend[3]
    #data_cont$start_s_detrend[1] <- data_cont$start_s_detrend[2] - data_cont$onset_diff_2p_detrend[2]
  
    } else if(dyad >200 &  dyad <300){
    
      
      data_cont <- data_cont %>% 
        group_by(hit_number_participant) %>%
        mutate(group_hit = (start_s[1]+start_s[2]) / 2)
      
      if(nrow(data_cont) %% 2 == 1){
        data_cont <- data_cont[-nrow(data_cont),]
      }
      
      temp <- astsa::detrend(!is.na(data_cont$group_hit[seq(1,length(data_cont$group_hit),2)]))
      temp <- unname(rep(temp, each = 2))
      
      data_cont$start_s_detrend <- data_cont$start_s - temp
      
      data_cont <- data_cont %>% 
        ungroup() %>%
        mutate(onset_diff_2p_detrend = start_s_detrend - lag(start_s_detrend)) %>%
        group_by(participant) %>%
        mutate(onset_diff_1p_detrend = start_s_detrend - lag(start_s_detrend, 1)) 
  }  
  
  
  data <- bind_rows(data_start, data_cont) 

  return(data)
}
