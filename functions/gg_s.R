gg_s <- function(data, loess = F){
  data$participant <- as.factor(data$participant)
  data$hit_number <- 1:nrow(data)
  
  gg <- ggplot(data=data, aes(x=start_s, y=onset_diff_1p, group=participant)) +
    geom_line(aes(color=participant))+
    geom_point(aes(color=participant))+
    geom_vline(xintercept = 15,size=1)+
    ggtitle(paste0(dyad, " Trial #",  trial))
  
  if(loess == F){
    return(gg)
  } else {
    return(gg + geom_smooth())
  }
  
}
