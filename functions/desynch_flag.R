
desynch_flag <- function(data){
  data$desynch_flag1 <- ifelse (data$onset_diff_1p_detrend > 1.25 | data$onset_diff_1p_detrend < .75,
                                T, F)
  
  
  data$desynch_flag2 <- ifelse (data$onset_diff_1p_detrend > median(data$onset_diff_1p_detrend, na.rm = T)+4*sd(data$onset_diff_1p_detrend, na.rm = T) |
                                data$onset_diff_1p_detrend < median(data$onset_diff_1p_detrend, na.rm = T)-4*sd(data$onset_diff_1p_detrend, na.rm = T),
                                T, F)
  
  data$desynch_flag3 <- ifelse (data$onset_diff_1p_detrend > median(data$onset_diff_1p_detrend, na.rm = T)+5*sd(data$onset_diff_1p_detrend, na.rm = T) |
                                data$onset_diff_1p_detrend < median(data$onset_diff_1p_detrend, na.rm = T)-5*sd(data$onset_diff_1p_detrend, na.rm = T),
                                T, F)
  
  data$desynch <- ifelse(data$desynch_flag1 == T |
                         data$desynch_flag2 == T |
                         data$desynch_flag3 == T,
                         T, F)
  
  return(data)
}

# The plan:
#   Collect data from file load_data function
#   Identify outliers by making an if statement: if within range, don't mark; if outside, mark
#     Do via: 
#         fixed range (x,-x), 
#         variable range by mean (mean+x, mean-x), 
#         variable range by sd (mean+xsd, mean-xsd), 
#         IQR (Q3+1.5IQR, Q1-1.5IQR)
#   Flag desynch when n(outliers) > 0 with "desynch = TRUE or FALSE"
# Write 3 desynch flags and 'all' desynch
# 
# 
# data$onset_diff_1p_detrend <- na.omit(data$onset_diff_1p_detrend)
# min <- min(data$onset_diff_1p_detrend, na.rm=T)
# max <- max(data$onset_diff_1p_detrend, na.rm=T)
# psych::describe(data$onset_diff_1p_detrend)
# #fixed range of 0.75 to 1.25:
# if(min < 0.75){
#   desynch <- TRUE
# }else if(max > 1.25) {
#   desynch <- TRUE
# }else desynch1 <- FALSE
# 
# #fixed range by trial mean +/- 0.25s
# if(min < mean(data$onset_diff_1p_detrend)-0.25){
#   desynch <- TRUE
# }else if(max > mean(data$onset_diff_1p_detrend)+0.25){
#   desynch <- TRUE
# }else desynch2 <- FALSE
# 
# #fixed range by trial mean +/- 3(sd)
# if(min < (mean(data$onset_diff_1p_detrend)-4*sd(data$onset_diff_1p_detrend))){
#   desynch <- TRUE
# }else if(max > (mean(data$onset_diff_1p_detrend)+4*sd(data$onset_diff_1p_detrend))){
#   desynch <- TRUE
# }else desynch3 <- FALSE
# 
# #fixed range by IQR
# if(min < (mean(data$onset_diff_1p_detrend)-3*sd(data$onset_diff_1p_detrend))){
#   desynch <- TRUE
# }else if(max > (mean(data$onset_diff_1p_detrend)+3*sd(data$onset_diff_1p_detrend))){
#   desynch <- TRUE
# }else desynch3 <- FALSE
# 
# Q1 <- unname(quantile(data$onset_diff_1p_detrend)[1]) #q1
# Q2 <- unname(quantile(data$onset_diff_1p_detrend)[2]) #q2
# Q3 <- unname(quantile(data$onset_diff_1p_detrend)[3]) #q3
# Q4 <- unname(quantile(data$onset_diff_1p_detrend)[4]) #q4
