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
      ggtitle(paste0(dyad, " Trial #",  trial))+
      theme_bw()
  } else if(detrend == TRUE){
    require(gridExtra)
    
    gg1 <- ggplot(data=data %>% filter(start_s >= synch_phase_end), aes(x=start_s, y=onset_diff_1p, group=as.factor(participant))) +
      geom_line(aes(color=participant))+
      geom_point(aes(color=participant))+
      geom_vline(xintercept = synch_phase_end, size=1)+
      ggtitle(paste0(dyad, " Trial #",  trial, ": Raw"))+
      theme_bw()
    
    gg2 <- ggplot(data=data, aes(x=start_s_detrend, y=onset_diff_1p_detrend, group=participant)) +
      geom_line(aes(color=participant))+
      geom_point(aes(color=participant))+
      geom_vline(xintercept = synch_phase_end, size=1)+
      ggtitle(paste0(dyad, " Trial #",  trial, ": Detrended"))+
      theme_bw()
    
    grid_plot <- grid.arrange(gg1, gg2, ncol=2)
    return(grid_plot)
  }
  
 
  
  if(loess == F){
    return(gg)
  } else {
    return(gg + geom_smooth())
  }
  
  # gg + 
  #   geom_vline(xintercept = measures, linetype = "dashed")
  
}
