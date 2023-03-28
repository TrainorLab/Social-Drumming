generate_continuation_measures <- function(data){
continuation <- data %>% filter(start_s > 31.6)
continuation <- recalc_onsets(continuation)

idx <- seq(1, nrow(continuation), 4)  

measures <- continuation %>% filter(hit_number_participant %in% idx) %>%
  group_by(hit_number_participant) %>%
  mutate(group_measure = (start_s[1]+start_s[2]) / 2) %>%
  filter(participant == 1) %>%
  pull(group_measure)
measures <- c( seq(0,32,4), measures)
}
