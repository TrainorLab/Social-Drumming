rm(list = ls())
library(tidyverse)
library(brms)
rds_files <- list.files(path = "C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\brms_fits\\", pattern = "\\.rds$", full.names = TRUE)
bfp_files <- list.files(path = "C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\brms_fits\\BFp\\", pattern = "\\.rds$", full.names = TRUE)

# Define a function to read each RDS file and assign it to a variable with the filename (without .rds extension)
read_and_assign_rds <- function(file_path) {
  # Extract the filename without extension
  variable_name <- tools::file_path_sans_ext(basename(file_path))
  # Read the RDS file
  data <- readRDS(file_path)
  # Assign the data to a variable with the filename
  assign(variable_name, data, envir = .GlobalEnv)
}
walk(rds_files, read_and_assign_rds)
walk(bfp_files, read_and_assign_rds)

models <- ls()[str_detect(ls(), "fit")]

for(i in 1:length(models)){
  print(get(models[i]))
  readline()
}



#Bayes Factor Calculations with bayestestR
######
bfp_q1 <- bayestestR::bayesfactor_parameters(fit3_q1, null = 0)
bfp_q2 <- bayestestR::bayesfactor_parameters(fit3_q2, null = 0)
bfp_q3 <- bayestestR::bayesfactor_parameters(fit3_q3, null = 0)
bfp_q4 <- bayestestR::bayesfactor_parameters(fit3_q4, null = 0)
bfp_q5 <- bayestestR::bayesfactor_parameters(fit3_q5, null = 0)
bfp_q6 <- bayestestR::bayesfactor_parameters(fit3_q6, null = 0)

bfm_q1 <- bayestestR::bayesfactor_models(fit2_q1, fit3_q1)
bfm_q2 <- bayestestR::bayesfactor_models(fit2_q2, fit3_q2)
bfm_q3 <- bayestestR::bayesfactor_models(fit2_q3, fit3_q3)
bfm_q4 <- bayestestR::bayesfactor_models(fit2_q4, fit3_q4)
bfm_q5 <- bayestestR::bayesfactor_models(fit2_q5, fit3_q5)
bfm_q6 <- bayestestR::bayesfactor_models(fit2_q6, fit3_q6)

bfps <- ls()[str_detect(ls(), "bfp_q")]
BFp <- c()

bfms <- ls()[str_detect(ls(), "bfm_q")]
BFm <- c()


for(i in 1:6){
  print(paste0("Question ", i, ":"))
  BFp[i] <- print(exp(get(bfps[i])$log_BF)[nrow(get(bfps[i]))])
  assign(paste0("gg_bfp_", i) , print(plot(get(bfps[i])) + ggtitle(paste0("Question ", i))) +
    theme_bw())
  #saveRDS(get(bfps[i]), paste0("C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\brms_fits\\BFp\\", bfps[i],".rds"))
  readline()
}

scale <- 3 
png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\Bayes_Factor_Plots.png", width = 800*scale, height = 480*scale, res = 72*scale)
grid.arrange(gg_bfp_1, gg_bfp_2, gg_bfp_3, gg_bfp_4, gg_bfp_5, gg_bfp_6, nrow = 3)
dev.off()



for(i in 1:6){
  print(paste0("Question ", i, ":"))
  BFm[i] <- print(exp(get(bfms[i])$log_BF)[nrow(get(bfms[i]))])
#  print(plot(get(bfps[i])) + ggtitle(paste0("Question ", i)))
  readline()
}

BFp
BFm

q2_fits <- ls()[str_detect(ls(), "fit2")]
q3_fits <- ls()[str_detect(ls(), "fit3")]

fit2_prior_list <- list()
fit3_prior_list <- list()

for(i in 1:6){
  fit2_prior_list[[i]] <- prior_summary(get(q2_fits[i]))
}
for(i in 1:6){
  fit3_prior_list[[i]] <- prior_summary(get(q3_fits[i]))
  print(fit3_prior_list[[i]])
  readline()
}

#Bayes Factors with brms;:bayes_factor
##### 

bfb_q1 <- brms::bayes_factor(fit3_q1, fit2_q1)
bfb_q2 <- brms::bayes_factor(fit3_q2, fit2_q2)
bfb_q3 <- brms::bayes_factor(fit3_q3, fit2_q3)
bfb_q4 <- brms::bayes_factor(fit3_q4, fit2_q4)
bfb_q5 <- brms::bayes_factor(fit3_q5, fit2_q5)
bfb_q6 <- brms::bayes_factor(fit3_q6, fit2_q6)

bfbs <- ls()[str_detect(ls(), "bfb_q")]
BFb3 <- c()

for(i in 1:6){
  print(paste0("Question ", i, ":"))
  BFb3[i] <- print((get(bfbs[i])$bf))
  #  print(plot(get(bfps[i])) + ggtitle(paste0("Question ", i)))
  readline()
}

bfbs <- data.frame(A = BFb, b = BFb2, c = BFb3)


fit3s <- ls()[str_detect(ls(), "fit3")]

for(i in 1:length(fit3s)){
 print(get(fit3s[i]))
 #print(prior_summary(get(fits[i])))
 readline()
}

for(i in 1:length(fit3s)){
  print(mcmc_plot(get(fit3s[i]), type = 'neff'))
  #print(prior_summary(get(fits[i])))
  readline()
}

pp_check_ecdf <- function(fit, ncat, ndraws = 20){
  pp_check(fit, type = "ecdf_overlay", ndraws = ndraws) +
    scale_x_continuous("y", breaks = 1:ncat) +
    scale_y_continuous(NULL, breaks = NULL, expand = expansion(mult = c(0, 0.05))) +
    ggtitle(fit$formula) +
    theme(legend.background = element_blank(),
          legend.position = c(.9, .8))
  
}

##prior predictive checks
pp_check_q1_prior <- pp_check_ecdf(fit2_q1_prior, ndraws = 100, ncat = 6)
pp_check_q2_prior <- pp_check_ecdf(fit2_q2_prior, ndraws = 100, ncat = 6)
pp_check_q3_prior <- pp_check_ecdf(fit2_q3_prior, ndraws = 100, ncat = 7)
pp_check_q4_prior <- pp_check_ecdf(fit2_q4_prior, ndraws = 100, ncat = 7)
pp_check_q5_prior <- pp_check_ecdf(fit2_q5_prior, ndraws = 100, ncat = 7)
pp_check_q6_prior <- pp_check_ecdf(fit2_q6_prior, ndraws = 100, ncat = 5)

scale = 3
png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\prior_predictive_checks.png", width = 800*scale, height = 480*scale, res = 72*scale)
gridExtra::grid.arrange(pp_check_q1_prior, pp_check_q2_prior, pp_check_q3_prior,
                        pp_check_q4_prior, pp_check_q5_prior, pp_check_q6_prior)  
dev.off()

##posterior predictive checks
pp_check_q1 <- pp_check_ecdf(fit2_q1, ncat = 6)
pp_check_q2 <-pp_check_ecdf(fit2_q2, ncat = 6)
pp_check_q3 <-pp_check_ecdf(fit2_q3, ncat = 7)
pp_check_q4 <-pp_check_ecdf(fit2_q4, ncat = 7)
pp_check_q5 <-pp_check_ecdf(fit2_q5, ncat = 7)
pp_check_q6 <-pp_check_ecdf(fit2_q6, ncat = 5)


png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\posterior_predictive_checks.png", width = 800*scale, height = 480*scale, res = 72*scale)
gridExtra::grid.arrange(pp_check_q1, pp_check_q2, pp_check_q3,
                        pp_check_q4, pp_check_q5, pp_check_q6)  
dev.off()

# pp_check_q1_fit3 <- pp_check_ecdf(fit3_q1, ncat = 6)
# pp_check_q2_fit3 <-pp_check_ecdf(fit3_q2, ncat = 6)
# pp_check_q3_fit3 <-pp_check_ecdf(fit3_q3, ncat = 7)
# pp_check_q4_fit3 <-pp_check_ecdf(fit3_q4, ncat = 7)
# pp_check_q5_fit3 <-pp_check_ecdf(fit3_q5, ncat = 7)
# pp_check_q6_fit3 <-pp_check_ecdf(fit3_q6, ncat = 5)
# 
# gridExtra::grid.arrange(pp_check_q1_fit3, pp_check_q2_fit3, pp_check_q3_fit3,
#                         pp_check_q4_fit3, pp_check_q5_fit3, pp_check_q6_fit3)  

model_out_dir <- "C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\brms_fits\\html_model_output\\"
sjPlot::tab_model(fit3_q1, transform = NULL, file = paste0(model_out_dir, "fit3_q1_model_out.html"))


for(i in 1:length(fit3s)){
  print(get(fit3s[i]))
  sjPlot::tab_model(get(fit3s[i]), transform = NULL, file = paste0(model_out_dir, fit3s[i], "_model_out.html"))
}





rm(pp_check_q1, pp_check_q2, pp_check_q3, pp_check_q4, pp_check_q5, pp_check_q6,
   pp_check_q1_prior, pp_check_q2_prior, pp_check_q3_prior, pp_check_q4_prior, pp_check_q5_prior, pp_check_q6_prior,
   fit2_q1_prior, fit2_q2_prior, fit2_q3_prior, fit2_q4_prior, fit2_q5_prior, fit2_q6_prior)
