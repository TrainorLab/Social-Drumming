add_initial_row <- function(data, p, t){
  initial_row <- tibble(participant = p, 
                        start_s = t)
  data <- data %>% 
    ungroup() %>%
    add_row(initial_row, .before = 1)
  
  data <- data %>% group_by(participant)
}
