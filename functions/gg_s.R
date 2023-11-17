gg_s <- function(data, detrend = F, loess = F){
  data$participant <- as.factor(data$participant)
  data$hit_number <- 1:nrow(data)
  
  if(dyad <200){
    synch_phase_end <- 16
  } else if(dyad >200 &  dyad <300){
    synch_phase_end <- 32
  }
  
  if(detrend == FALSE){
    gg <- ggplot(data=data, aes(x=start_s, y=onset_diff_1p, group=participant)) +
      geom_line(aes(color=participant))+
      geom_point(aes(color=participant))+
      geom_vline(xintercept = synch_phase_end, size=1)+
      labs(title = paste0(dyad, " Trial #",  trial), x = "Onset Time (s)", y = "Inter-Onset Interval (Per Person)",
           group = "Participant")+
      theme_bw()
  } else if(detrend == TRUE){

    gg <- ggplot(data=data, aes(x=start_s_detrend, y=onset_diff_1p_detrend, group=participant)) +
      geom_line(aes(color=participant))+
      geom_point(aes(color=participant))+
      geom_vline(xintercept = synch_phase_end, size=1)+
      labs(title = paste0("Detrended:", dyad, " Trial #",  trial), x = "Onset Time (s)", y = "Inter-Onset Interval (Per Person)",
           group = "Participant")+
      theme_bw()
    
    #grid_plot <- grid.arrange(gg1, gg2, ncol=2)
      }
 
  if(loess == F){
    return(gg)
  } else {
    return(gg + geom_smooth())
  }
  

}
