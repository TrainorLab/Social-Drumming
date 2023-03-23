gg_hits <- function(data){
  data$participant <- as.factor(data$participant)
  data$hit_number <- 1:nrow(data) 
  
  ggplot(data=data, aes(x=hit_number_participant, y=onset_diff_1p, group=participant)) +
    geom_line(aes(color=participant))+
    geom_point(aes(color=participant))+
    geom_vline(xintercept = 15,size=1)
}
