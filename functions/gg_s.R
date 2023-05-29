gg_s <- function(data, loess = F){
  data$participant <- as.factor(data$participant)
  data$hit_number <- 1:nrow(data)
  
  if(dyad <200){
    synch_phase_end <- 16
  } else if(dyad >200 &  dyad <300){
    synch_phase_end <- 32
  }
  
  gg <- ggplot(data=data, aes(x=start_s, y=onset_diff_1p, group=participant)) +
    geom_line(aes(color=participant))+
    geom_point(aes(color=participant))+
    geom_vline(xintercept = synch_phase_end, size=1)+
    ggtitle(paste0(dyad, " Trial #",  trial))+
    theme_bw()
  
  if(loess == F){
    return(gg)
  } else {
    return(gg + geom_smooth())
  }
  
  # gg + 
  #   geom_vline(xintercept = measures, linetype = "dashed")
  
}
