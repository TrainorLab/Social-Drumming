InsertMissedHits <- function(data){
  skips <- which(data$skip_flag==1)
  p_skip <- (data[skips[1:length(skips)],1])
  new_p <- ifelse(p_skip$participant == 2, 1, 
                  ifelse(p_skip$participant == 1, 2, NA))
  
  if(length(skips) == 0){
    data_full <- data
  }
  else if(length(skips) == 1){
    data_cut1 <- data[1:(skips[1] - 1),]
    
    new_row1 <- tibble(participant=new_p[1],
                       onset_diff_2p=data$roll_2p[skips[1] - 1],
                       #onset_diff_1p=data$roll_1p[60],
                       roll_2p=data$roll_2p[skips[1] - 1])
    
    
    data_cut1 <- data_cut1 %>%
      rows_insert(new_row1, by = names(new_row1))
    
    data_til_end <- data[skips[1]:nrow(data),]
    data_full <- rbind(data_cut1, data_til_end)
  } 
  else if(length(skips) == 2){
    
    data_cut1 <- data[1:skips[1] - 1,]
    
    new_row1 <- tibble(participant=new_p[1],
                       onset_diff_2p=data$roll_2p[skips[1] - 1],
                       #onset_diff_1p=data$roll_1p[60],
                       roll_2p=data$roll_2p[skips[1] - 1])
    
    data_cut1 <- data_cut1 %>%
      rows_insert(new_row1, by = names(new_row1))
    
    data_cut2 <- data[skips[1]:(skips[2]-1),]
    
    new_row2 <- tibble(participant=new_p[2],
                       onset_diff_2p=data_cut2$roll_2p[nrow(data_cut2)],
                       #onset_diff_1p=data$roll_1p[8],
                       roll_2p=data_cut2$roll_2p[nrow(data_cut2)])
    
    data_cut2 <- data_cut2 %>%
      rows_insert(new_row2, by = names(new_row2))
    
    data_til_end <- data[(skips[2]):nrow(data),]
    
    data_full <- rbind(data_cut1, data_cut2, data_til_end)
  }
  else if(length(skips) >= 3){
    
    #cut the data frame from the start to the first missed hit flag
    data_cut1 <- data[1:skips[1] - 1,]
    
    #create new row with info about which participant missed, imputed value
    #and lag
    new_row1 <- tibble(participant=new_p[1],
                       onset_diff_2p=data$roll_2p[skips[1] - 1],
                       #onset_diff_1p=data$roll_1p[60],
                       roll_2p=data$roll_2p[skips[1] - 1])
    
    #insert the newly created row at the bottom of the cut data frame
    new_data_cut1 <- data_cut1 %>%
      rows_insert(new_row1, by = names(new_row1))
    
    # Insert as many rows as needed (as many missed hits there are)
    i = 1
    while(i < length(skips)){  
      
      #cut the data at the right spot, and name according to missed hit index
      assign(paste0("data_cut", (i+1)), 
              data[skips[i]:(skips[i+1]-1),])
      i = i + 1
    }  
    
    #Create the new rows to append to the cuts created above
    i = 1
    while(i < length(skips)){
      
      attach(get(paste0("data_cut", (i+1))))
      
      assign(paste0("new_row", (i+1)), 
             tibble(participant=new_p[i + 1],
                    onset_diff_2p= get("roll_2p")[nrow(get(paste0("data_cut", (i+1))))],
                    #onset_diff_1p=data$roll_1p[8],
                    roll_2p=get("roll_2p")[nrow(get(paste0("data_cut", (i+1))))]))
      
      detach(get(paste0("data_cut", (i+1))))
      
      i = i + 1
    }
    
    #Insert the new rows to their corresponding data frame
    for(i in 1:length(skips)){
      if((i + 1) <= length(skips) ){
        assign(paste0("new_data_cut", (i+1)), 
               rows_insert(get(paste0("data_cut", i+1)), 
                           get(paste0("new_row", (i+1))), 
                           by = names(get(paste0("new_row", (i +1))))))
      } 
    }
    
    #bind all cuts together, accommodating different size
    #env_print()  
    #  If you want to debug this section, you will need to temporarily change the envir
    start_block <- do.call(rbind,lapply(ls(patt="new_data_cut"), get, envir = environment()))
     
    data_til_end <- data[tail(skips, n = 1):nrow(data),]
    
    data_full <- rbind(start_block, data_til_end)  
  }
  return(data_full)
}
