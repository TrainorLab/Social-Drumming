trim_end <- function(data){
  
  if(dyad <200 && !dyad  %in% c(101:105, 118:119)){
    cont_phase_end <- 76
  } else if(dyad %in% c(101:105, 118:119)){
    cont_phase_end <- 61
  }
  else if(dyad == 202){
    cont_phase_end <- 92
  }
  else if(dyad >200 &  dyad <300){
    cont_phase_end <- 95
  }
  
  data <- data %>% filter(start_s < cont_phase_end)
  
}
