modify_individual_trial <- function(data){
  if(dyad == 202 && trial %in% c(1,2)){
    data <- data %>% filter(start_s < 90.5)
  }
  return(data)
}
