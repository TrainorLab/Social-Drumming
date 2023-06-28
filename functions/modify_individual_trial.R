modify_individual_trial <- function(data){
  
  if(dyad == 110 && trial == 1){
    
    data <- data %>% filter(start_s > 22)
    data <- recalc_onsets(data)
    data <- align_first_hit(data)
    data <- trim_end(data)
  }
  
  
  
  
  if(dyad == 202 && trial %in% c(1,2)){
    data <- data %>% filter(start_s < 90.5)
  } 
    
  if(dyad == 203 & trial == 3){
    data <- remove_hit(data, 1, 76)
  }
  
  if(dyad == 208 & trial == 3){
    data <- data %>% filter(start_s >= 11)
  }
  
  if(dyad == 209 & trial == 1){
    data <- data %>% filter(start_s >= 9)
  }
  
  if(dyad == 209 & trial == 2){
    new_row <- data.frame(start_s = 46.4, participant = 2, imputed = 1)
    
    data <- data %>%
      ungroup() %>%
      add_row(new_row, .before = 69)
  }
  
  if(dyad == 209 & trial == 3){
    data <- data %>% 
      filter(start_s <= 91)
    data <- recalc_onsets(data)
    data <- clean_all_missed(data, threshold = .5)
  }
  
  if(dyad == 209 & trial == 4){
    data <- data %>% 
      filter(start_s <= 94)
    data <- recalc_onsets(data)
    data <- clean_all_missed(data, threshold = .5)
  }
  
  if(dyad == 210 & trial == 2){
    data <- data %>%
      filter(start_s >= 32)
    data$onset_diff_1p[1] <- NA
    data$onset_diff_2p[1] <- NA
  }
  
  if(dyad == 210 & trial == 3){
    
    data <- data %>%
      filter(start_s < 90)
    data <- recalc_onsets(data)
    new_row1 <- data.frame(start_s = 72.5, participant = 1, imputed = 1)
    new_row2 <- data.frame(start_s = 72.5, participant = 2, imputed = 1)
    
    data <- data %>%
      ungroup() %>%
      add_row(new_row1, .after = 90) %>%
      add_row(new_row2, .after = 90)
    
    data <- recalc_onsets(data)
    data <- clean_all_missed(data)
    
  }
  
  if(dyad == 210 & trial == 4){
    
    new_row1 <- data.frame(start_s = 56.2, participant = 1, imputed = 1)
    new_row2 <- data.frame(start_s = 56.2, participant = 2, imputed = 1)
    
    data <- data %>%
      ungroup() %>%
      add_row(new_row1, .after = 47) %>%
      add_row(new_row2, .after = 47)
    
    data <- recalc_onsets(data)
    
    data <- clean_all_missed(data, threshold = .5)

  }

  if(dyad == 211 & trial == 1){
    data <- recalc_onsets(data)
    data <- data %>%
      filter(start_s > 8)
    data <- recalc_onsets(data)
  }
 
  return(data)
}

