library(tidyverse)
library(brms)

rm(list = ls())
# trial_df <- read_rds("X:\\Sean M\\Social_Drumming\\trial_df.rds")
# avg_df <- read_rds("X:\\Sean M\\Social_Drumming\\trial_avgs_df.rds")
beh <- read_rds("X:\\Sean M\\Social_Drumming\\beh_df.rds")

beh <- beh %>% filter(Exclude == FALSE)
beh2 <- beh %>% filter(condition != "Alone")


#####
# default prior - for all theta cutoffs, written out explicitly
#priors <- prior(normal(0, 4), class="Intercept")

# no obs at 1, only 2-7, so rescale
beh2$Likert_Q1 <- beh2$Likert_Q1 - 1
# change the 4.5 to a 4 for modeling ease
beh2$Likert_Q4 <- as.integer(ifelse(beh2$Likert_Q4 == 4.5, 4, beh2$Likert_Q4))

#Shift down
beh2$Likert_Q2 <- ifelse(beh2$Likert_Q2 == 1, 2, beh2$Likert_Q2)
beh2$Likert_Q2 <- beh2$Likert_Q2 - 1

#Shift down
beh2$Likert_Q6 <- beh2$Likert_Q6 - 2

#Q1

priors <- c(prior(normal(-1, 2), coef = 1, class="Intercept"),
            prior(normal(-.5, 2), coef = 2, class="Intercept"),
            prior(normal(0, 2), coef = 3, class="Intercept"),
            prior(normal(.5, 2), coef = 4, class="Intercept"),
            prior(normal(1, 2), coef = 5, class="Intercept"))

priors <- c(priors, 
            prior(normal(0,1), class = "b"))

fit3_q1_narrow <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q1 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 11000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q1_narrow.rds")

fit3_q2_narrow <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q2 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q2_narrow.rds")


#
priors <- c(prior(normal(-1.5, 2), coef = 1, class="Intercept"),
            prior(normal(-1, 2), coef = 2, class="Intercept"),
            prior(normal(-.5, 2), coef = 3, class="Intercept"),
            prior(normal(0, 2), coef = 4, class="Intercept"),
            prior(normal(.5, 2), coef = 5, class="Intercept"),
            prior(normal(1, 2), coef = 6, class="Intercept"))

priors <- c(priors, 
            prior(normal(0,1), class = "b"))

fit3_q3_narrow <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q3 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q3_narrow.rds")


priors <- c(prior(normal(-2, 2), coef = 1, class="Intercept"),
            prior(normal(-1.5, 2), coef = 2, class="Intercept"),
            prior(normal(-1, 2), coef = 3, class="Intercept"),
            prior(normal(-.5, 2), coef = 4, class="Intercept"),
            prior(normal(0, 2), coef = 5, class="Intercept"),
            prior(normal(.5, 2), coef = 6, class="Intercept"))
priors <- c(priors, 
            prior(normal(0,1), class = "b"))

fit3_q4_narrow <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q4 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q4_narrow.rds")



priors <- c(prior(student_t(3, -2, 2), coef = 1, class="Intercept"),
            prior(student_t(3, -1.5, 2), coef = 2, class="Intercept"),
            prior(student_t(3, -1, 2), coef = 3, class="Intercept"),
            prior(student_t(3, -.5, 2), coef = 4, class="Intercept"),
            prior(student_t(3, 0, 2), coef = 5, class="Intercept"),
            prior(student_t(3, .5, 2), coef = 6, class="Intercept"))

priors <- c(priors, 
            prior(normal(0,1), class = "b"))


fit3_q5_narrow <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q5 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q5_narrow.rds")

priors <- c(prior(normal(-1, 2), coef = 1, class="Intercept"),
            prior(normal(-.5, 2), coef = 2, class="Intercept"),
            prior(normal(0, 2), coef = 3, class="Intercept"),
            prior(normal(.5, 2), coef = 4, class="Intercept"))

priors <- c(priors, 
            prior(normal(0,1), class = "b"))

fit3_q6_narrow <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q6 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q6_narrow.rds")

bfp_q1_narrow <- bayestestR::bayesfactor_parameters(fit3_q1_narrow, null = 0)
bfp_q2_narrow <- bayestestR::bayesfactor_parameters(fit3_q2_narrow, null = 0)
bfp_q3_narrow <- bayestestR::bayesfactor_parameters(fit3_q3_narrow, null = 0)
bfp_q4_narrow <- bayestestR::bayesfactor_parameters(fit3_q4_narrow, null = 0)
bfp_q5_narrow <- bayestestR::bayesfactor_parameters(fit3_q5_narrow, null = 0)
bfp_q6_narrow <- bayestestR::bayesfactor_parameters(fit3_q6_narrow, null = 0)

bfps <- ls()[str_detect(ls(), "bfp")]
bfps <- c("bfp_q1_narrow", "bfp_q2_narrow", "bfp_q3_narrow", "bfp_q4_narrow", "bfp_q5_narrow", "bfp_q6_narrow")

for(i in 1:6){
  print(paste0("Question ", i, ":"))
  print(exp(get(bfps[i])$log_BF)[nrow(get(bfps[i]))])
  saveRDS(get(bfps[i]), paste0("C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\brms_fits\\BFp\\", bfps[i],".rds"))
  readline()
}



loo3_q1_narrow <- loo(fit3_q1_narrow, moment_match = T)
loo3_q2_narrow <- loo(fit3_q2_narrow, moment_match = T)
loo3_q3_narrow <- loo(fit3_q3_narrow, moment_match = T)
loo3_q4_narrow <- loo(fit3_q4_narrow, moment_match = T)
loo3_q5_narrow <- loo(fit3_q5_narrow, moment_match = T)
loo3_q6_narrow <- loo(fit3_q6_narrow, moment_match = T)

loos <- c("loo3_q1_narrow", "loo3_q2_narrow", "loo3_q3_narrow", "loo3_q4_narrow", "loo3_q5_narrow", "loo3_q6_narrow")

for(i in 1:6){
  saveRDS(get(loos[i]), paste0("C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\brms_fits\\loo\\", loos[i],".rds"))
}
