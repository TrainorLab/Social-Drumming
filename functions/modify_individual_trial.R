modify_individual_trial <- function(data){
  if(dyad == 202 && trial %in% c(1,2)){
    data <- data %>% filter(start_s < 90.5)
  } 
    
  if(dyad == 203 & trial == 3){
    data <- remove_hit(data, 1, 76)
  }
  
  if(dyad == 208 & trial == 3){
    data <- data %>% filter(start_s >= 11)
  }
    
  return(data)
}
