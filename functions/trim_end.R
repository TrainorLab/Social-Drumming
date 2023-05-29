trim_end <- function(data){
  
  if(dyad <200){
    cont_phase_end <- 76
  } else if(dyad >200 &  dyad <300){
    cont_phase_end <- 95
  }
  
  data <- data %>% filter(start_s < cont_phase_end)
  
}
