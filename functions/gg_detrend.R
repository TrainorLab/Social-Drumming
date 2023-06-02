gg_detrend <- function(data){
  data$participant <- as.factor(data$participant)
  data$hit_number <- 1:nrow(data)
  
  if(dyad <200){
    synch_phase_end <- 16
  } else if(dyad >200 &  dyad <300){
    synch_phase_end <- 32
  }
  
    gg <- ggplot(data=data, aes(x=start_s_detrend, y=onset_diff_1p_detrend, group=participant)) +
      geom_line(aes(color=participant))+
      geom_point(aes(color=participant))+
      geom_vline(xintercept = synch_phase_end, size=1)+
      ggtitle(paste0(dyad, " Trial #",  trial, ": Detrended"))+
      theme_bw()
    
  return(gg)
}