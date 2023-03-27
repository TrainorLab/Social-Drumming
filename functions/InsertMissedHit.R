InsertMissedHit <- function(data, n_skipped){
  
  if(n_skipped == 1){
    
    skips <- which(data$skip_flag==1)
    new_p <- data[skips[1],]$participant
    
    new_row1 <- tibble(participant=new_p,
                       onset_diff_1p=data$roll_1p[skips[1] - 1],
                       roll_1p=data$roll_1p[skips[1] - 1],
                       imputed = 1)
    
    if(is.na(new_row1$onset_diff_1p[1])){
      new_row1$onset_diff_1p[1] <- 1
    }
    
    data <- data %>% ungroup() %>%
      add_row(new_row1, .before = (skips[1])) %>%
      group_by(participant)
    
    data <- recalc_onsets(data)
    
  } else if(n_skipped == 2){
    
    skips <- which(data$double_skip_flag==1)
    new_p <- data[skips[1],]$participant
    
    new_row1 <- tibble(participant=new_p,
                       onset_diff_1p=data$roll_1p[skips[1] - 1],
                       roll_1p=data$roll_1p[skips[1] - 1],
                       imputed = 1)
    
    if(is.na(new_row1$onset_diff_1p[1])){
      new_row1$onset_diff_1p[1] <- 1
    }
    
    
    data <- data %>% ungroup() %>%
      add_row(new_row1, .before = (skips[1]))
    
    
    data <- data %>% 
      group_by(participant) %>%
      mutate(start_s = ifelse(is.na(start_s), onset_diff_1p + lag(start_s), start_s))
    
    
    #data <- recalc_onsets(data)
    
    new_row2 <- tibble(participant = new_p,
                       onset_diff_1p = data$roll_1p[skips[1]],
                       start_s = data$roll_1p[skips[1]] + data$start_s[skips[1]],
                       imputed = 1)
    
    
    data <- data %>% ungroup() %>% 
      add_row(new_row2, .before = (skips[1] + 1))
    
    data <- recalc_onsets(data)
    
  }
}