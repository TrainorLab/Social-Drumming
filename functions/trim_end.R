trim_end <- function(data){
  data <- data %>% filter(start_s < 95)
}
