InsertMissedHit <- function(data, n_skipped){
  
  if(n_skipped == 1){
    
    skips <- which(data$skip_flag==1)
    new_p <- data[skips[1],]$participant
    
    data_cut1 <- data[1:(skips[1] - 1),]
    
    new_row1 <- tibble(participant=new_p,
                       onset_diff_1p=data$roll_1p[skips[1] - 1],
                       roll_1p=data$roll_1p[skips[1] - 1],
                       imputed = 1)
    
    if(is.na(new_row1$onset_diff_1p[1])){
      new_row1$onset_diff_1p[1] <- 1
    }
    
    
    data_cut1 <- data_cut1 %>% ungroup() %>%
      add_row(new_row1, .after = nrow(data_cut1)) %>%
      group_by(participant)
    
    # data_cut1 <- data_cut1 %>%
    #   rows_insert(new_row1, by = names(new_row1))
    
    data_til_end <- data[skips[1]:nrow(data),]
    data_full <- rbind(data_cut1, data_til_end)
    
    
  } else if(n_skipped == 2){
    
    skips <- which(data$double_skip_flag==1)
    new_p <- data[skips[1],]$participant
    
    data_cut1 <- data[1:(skips[1] - 1),]
    
    new_row1 <- tibble(participant=new_p,
                       onset_diff_1p=data$roll_1p[skips[1] - 1],
                       roll_1p=data$roll_1p[skips[1] - 1],
                       imputed = 1)
    
    if(is.na(new_row1$onset_diff_1p[1])){
      new_row1$onset_diff_1p[1] <- 1
    }
    
    
    data_cut1 <- data_cut1 %>%
      rows_insert(new_row1, by = names(new_row1))
    
    data_cut1 <- recalc_onsets(data_cut1)
    
    new_row2 <- tibble(participant = new_p,
                       onset_diff_1p = data_cut1$roll_1p[skips[1]],
                       start_s = data_cut1$roll_1p[skips[1]] + data_cut1$start_s[skips[1]],
                       imputed = 1)
    
    
    data_cut1 <- data_cut1 %>% 
      rows_insert(new_row2, by = names(new_row2))
    
    
    
    
    data_til_end <- data[skips[1]:nrow(data),]
    data_full <- rbind(data_cut1, data_til_end)
    
    data_full <- recalc_onsets(data_full)
    
  }
}
