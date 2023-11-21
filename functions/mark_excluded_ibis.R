mark_excluded_ibis <- function(data){
  data <- data %>% 
    group_by(hit_number_participant) %>%
    mutate(exclude_IBI = any(imputed == 1)) %>%
    ungroup() %>%
    mutate(exclude_IBI2 = dplyr::lag(imputed) == 1 | dplyr::lag(imputed, 2) == 1) %>%
    group_by(hit_number_participant) %>%
    mutate(exclude_IBI3 = any(exclude_IBI2) == T) %>%
    mutate(exclude_IBI = exclude_IBI3) %>%
    select(-exclude_IBI2, -exclude_IBI3)  
}