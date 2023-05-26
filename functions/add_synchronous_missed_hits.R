add_synchronous_missed_hits <- function(data, n_missed, where, participant){
  #" where" is the row that corresponds to other participant's hit you'd like to duplicate 
  for(n in 1:n_missed){
    data <- data %>% ungroup %>%
      add_row(start_s = data$start_s[where + 2*(n-1)], 
              participant = participant,
              imputed = 1,
              .before = where + 2*(n-1))  
  }
  return(data)
}