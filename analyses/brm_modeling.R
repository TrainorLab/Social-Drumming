library(tidyverse)
library(brms)

rm(list = ls())
# trial_df <- read_rds("X:\\Sean M\\Social_Drumming\\trial_df.rds")
# avg_df <- read_rds("X:\\Sean M\\Social_Drumming\\trial_avgs_df.rds")
beh <- read_rds("X:\\Sean M\\Social_Drumming\\beh_df.rds")

beh <- beh %>% filter(Exclude == FALSE)
beh2 <- beh %>% filter(condition != "Alone")

beh_long <- beh %>% 
  pivot_longer(names_to = "Q", values_to = "Likert_Score", cols = c("Likert_Q1", "Likert_Q2", "Likert_Q3", "Likert_Q4", "Likert_Q5", "Likert_Q6"))


# ggplot(data = beh_long %>% filter(Dyad < 300), aes(x = Likert_Score)) +
#   geom_histogram(bins = 7) +
my_colors <- RColorBrewer::brewer.pal(3, "Set2")[1:3]

#   facet_wrap(~Q) +
#   theme_bw()


beh_long$Likert_Score <- as.integer(ifelse(beh_long$Likert_Score == 4.5, 4, beh_long$Likert_Score))

scale = 3
png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\stacked_hist_all_groups.png", width = 800*scale, height = 480*scale, res = 72*scale)
ggplot(data = beh_long %>%
         mutate(Q = recode(Q,
                           "Likert_Q1" = "Unit",
                           "Likert_Q2" = "Same Team",
                           "Likert_Q3" = "Trust Before",
                           "Likert_Q4" = "Similar",
                           "Likert_Q5" = "Cooperated",
                           "Likert_Q6" = "Happy")), 
       aes(x = Likert_Score, fill = condition)) +
  geom_bar(position='stack') +
  scale_fill_manual(values=c(my_colors[3], my_colors[1], my_colors[2])) +
  facet_wrap(~Q) +
  theme_bw() +
  ggtitle("Stacked Histograms: Likert Question by Group") +
  labs(y= "Count", x = "Likert Score", fill = "Condition")
dev.off()  




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

# priors <- c(prior(normal(-1.5, 2), coef = 1, class="Intercept"),
#             prior(normal(-1, 2), coef = 2, class="Intercept"),
#             prior(normal(-.5, 2), coef = 3, class="Intercept"),
#             prior(normal(0, 2), coef = 4, class="Intercept"),
#             prior(normal(.5, 2), coef = 5, class="Intercept"))
tic <- Sys.time()
priors <- c(prior(normal(-1, 2), coef = 1, class="Intercept"),
            prior(normal(-.5, 2), coef = 2, class="Intercept"),
            prior(normal(0, 2), coef = 3, class="Intercept"),
            prior(normal(.5, 2), coef = 4, class="Intercept"),
            prior(normal(1, 2), coef = 5, class="Intercept"))


fit2_q1 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q1 ~ 1 + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95))
      #file = ".\\brms_fits\\fit2_q1.rds")


# pp_check(fit2_q1, type = "ecdf_overlay", ndraws = 50)
# #prior_summary(fit2_q1)
# print(fit2_q1)

priors <- c(priors, 
            prior(normal(0,2), class = "b"))

fit3_q1 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q1 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 11000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q1.rds")
  
#pp_check(fit3_q1, type = "ecdf_overlay", ndraws = 50)
# print(fit3_q1)
# #plot(fit3_q1)
# 
# fit3.2_q1

# plot_ordinal(fit_q1)
# plot_ordinal(fit2_q1)
plot_ordinal(fit3_q1) +
  theme_bw()

# loo_q1 <- loo(fit_q1)
loo2_q1 <- loo(fit2_q1)
loo3_q1 <- loo(fit3_q1)
# 
# loo_compare(loo2_q1, loo3_q1)
# #bf_q1 <- brms::bayes_factor(fit3_q1, fit_q1)
# (bf_q1 <- brms::bayes_factor(fit3_q1, fit2_q1))


##########
# priors <- c(prior(normal(-1.5, 2), coef = 1, class="Intercept"),
#             prior(normal(-1, 2), coef = 2, class="Intercept"),
#             prior(normal(-.5, 2), coef = 3, class="Intercept"),
#             prior(normal(0, 2), coef = 4, class="Intercept"),
#             prior(normal(.5, 2), coef = 5, class="Intercept"))

priors <- c(prior(normal(-1, 2), coef = 1, class="Intercept"),
            prior(normal(-.5, 2), coef = 2, class="Intercept"),
            prior(normal(0, 2), coef = 3, class="Intercept"),
            prior(normal(.5, 2), coef = 4, class="Intercept"),
            prior(normal(1, 2), coef = 5, class="Intercept"))


# fit_q2 <-
#   brm(data = beh2,
#       family = cumulative(probit),
#       Likert_Q2 ~ 1,
#       prior = priors, save_pars = save_pars(all = TRUE))
# 
# pp_check(fit2_q2, ndraws = 50)
# plot(fit_q2)
# print(fit_q2)

fit2_q2 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q2 ~ 1 + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit2_q2.rds")


# print(fit2_q2)
#plot(fit2_q2)

priors <- c(priors, 
            prior(normal(0,2), class = "b"))

fit3_q2 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q2 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q2.rds")

# pp_check(fit3_q2, type = "ecdf_overlay", ndraws = 50)
# print(fit3_q2)
# plot(fit3_q2)
# 
# plot_ordinal(fit_q2)
# plot_ordinal(fit2_q2)
# plot_ordinal(fit3_q2)

# loo_q2 <- loo(fit_q2)
loo2_q2 <- loo(fit2_q2, moment_match = T)
loo3_q2 <- loo(fit3_q2, moment_match = T)
# 
# loo_compare(loo2_q2, loo3_q2)
# (bf_q2 <- brms::bayes_factor(fit3_q2, fit2_q2))
######

# priors <- c(prior(normal(-2, 2), coef = 1, class="Intercept"),
#             prior(normal(-1.5, 2), coef = 2, class="Intercept"),
#             prior(normal(-1, 2), coef = 3, class="Intercept"),
#             prior(normal(-.5, 2), coef = 4, class="Intercept"),
#             prior(normal(0, 2), coef = 5, class="Intercept"),
#             prior(normal(.5, 2), coef = 6, class="Intercept"))
#                 
priors <- c(prior(normal(-1.5, 2), coef = 1, class="Intercept"),
            prior(normal(-1, 2), coef = 2, class="Intercept"),
            prior(normal(-.5, 2), coef = 3, class="Intercept"),
            prior(normal(0, 2), coef = 4, class="Intercept"),
            prior(normal(.5, 2), coef = 5, class="Intercept"),
            prior(normal(1, 2), coef = 6, class="Intercept"))


# fit_q3 <- brm(data = beh2,
#     family = cumulative(probit),
#     Likert_Q3 ~ 1,
#     prior = priors, save_pars = save_pars(all = TRUE))
# 
# prior_summary(fit_q3)
# 
# plot(fit_q3)
# print(fit_q3)

fit2_q3 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q3 ~ 1 + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit2_q3.rds")

# pp_check(fit2_q3, ndraws = 50)
# print(fit2_q3)
# plot(fit2_q3)

priors <- c(priors, 
            prior(normal(0,2), class = "b"))

fit3_q3 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q3 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q3.rds")


# print(fit3_q3)
# # plot(fit3_q3)
# # 
# # plot_ordinal(fit_q3)
# # plot_ordinal(fit2_q3)
# # plot_ordinal(fit3_q3)
# # 
# # loo_q3 <- loo(fit_q3)
loo2_q3 <- loo(fit2_q3, moment_match = T)
loo3_q3 <- loo(fit3_q3, moment_match = T)
# 
# loo_compare(loo2_q3, loo3_q3)
# (bf_q3 <- brms::bayes_factor(fit3_q3, fit2_q3))
#####


priors <- c(prior(normal(-2, 2), coef = 1, class="Intercept"),
            prior(normal(-1.5, 2), coef = 2, class="Intercept"),
            prior(normal(-1, 2), coef = 3, class="Intercept"),
            prior(normal(-.5, 2), coef = 4, class="Intercept"),
            prior(normal(0, 2), coef = 5, class="Intercept"),
            prior(normal(.5, 2), coef = 6, class="Intercept"))

# priors <- c(prior(normal(-1, 2), coef = 1, class="Intercept"),
#             prior(normal(-1, 2), coef = 2, class="Intercept"),
#             prior(normal(-.5, 2), coef = 3, class="Intercept"),
#             prior(normal(0, 2), coef = 4, class="Intercept"),
#             prior(normal(.5, 2), coef = 5, class="Intercept"),
#             prior(normal(1, 2), coef = 6, class="Intercept"))




# fit_q4 <- brm(data = beh2,
#               family = cumulative(probit),
#               Likert_Q4 ~ 1,
#               prior = priors, save_pars = save_pars(all = TRUE))
# 
# prior_summary(fit_q4)
# 
# plot(fit_q4)
# print(fit_q4)

fit2_q4 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q4 ~ 1 + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit2_q4.rds")

# print(fit2_q4)
#plot(fit2_q4)

priors <- c(priors, 
            prior(normal(0,2), class = "b"))


fit3_q4 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q4 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q4.rds")

# print(fit3_q4)
# plot(fit3_q4)
# 
# 
# plot_ordinal(fit_q4)
# plot_ordinal(fit2_q4)
# plot_ordinal(fit3_q4)
# 
# loo_q4 <- loo(fit_q4)
loo2_q4 <- loo(fit2_q4, moment_match = T)
loo3_q4 <- loo(fit3_q4, moment_match = T)
# 
# loo_compare(loo2_q4, loo3_q4)

#####
# priors <- c(prior(normal(-3, 3), coef = 1, class="Intercept"),
#             prior(normal(-2, 2.5), coef = 2, class="Intercept"),
#             prior(normal(-1, 2), coef = 3, class="Intercept"),
#             prior(normal(-.5, 2), coef = 4, class="Intercept"),
#             prior(normal(0, 2), coef = 5, class="Intercept"),
#             prior(normal(.5, 2), coef = 6, class="Intercept"))

priors <- c(prior(student_t(3, -2, 2), coef = 1, class="Intercept"),
            prior(student_t(3, -1.5, 2), coef = 2, class="Intercept"),
            prior(student_t(3, -1, 2), coef = 3, class="Intercept"),
            prior(student_t(3, -.5, 2), coef = 4, class="Intercept"),
            prior(student_t(3, 0, 2), coef = 5, class="Intercept"),
            prior(student_t(3, .5, 2), coef = 6, class="Intercept"))



# fit_q5 <- brm(data = beh2,
#               family = cumulative(probit),
#               Likert_Q5 ~ 1,
#               prior = priors, save_pars = save_pars(all = TRUE),
#               seed = 42, 
#               file = ".\\brms_fits\\fit2_q5.rds")
# 
# plot(fit_q5)
# print(fit_q5)


fit2_q5 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q5 ~ 1 + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit2_q5.rds")


# pp_check(fit2_q5, ndraws = 50)
# print(fit2_q5)
# plot(fit2_q5)

priors <- c(priors, 
            prior(normal(0,2), class = "b"))


fit3_q5 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q5 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q5.rds")


# print(fit3_q5)
# plot(fit3_q5)



# plot_ordinal(fit_q5)
# plot_ordinal(fit2_q5)
# plot_ordinal(fit3_q5)
# 
# loo_q5 <- loo(fit_q5)
loo2_q5 <- loo(fit2_q5, moment_match = T)
loo3_q5 <- loo(fit3_q5, moment_match = T)
# 
# loo_compare(loo2_q5, loo3_q5)

########
priors <- c(prior(normal(-1, 2), coef = 1, class="Intercept"),
            prior(normal(-.5, 2), coef = 2, class="Intercept"),
            prior(normal(0, 2), coef = 3, class="Intercept"),
            prior(normal(.5, 2), coef = 4, class="Intercept"))



# fit_q6 <- brm(data = beh2,
#               family = cumulative(probit),
#               Likert_Q6 ~ 1,
#               prior = priors, save_pars = save_pars(all = TRUE))
# 
# plot(fit_q6)
# print(fit_q6)

fit2_q6 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q6 ~ 1 + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit2_q6.rds")

# pp_check(fit2_q6, ndraws = 50)
# print(fit2_q6)
# plot(fit2_q6)


priors <- c(priors, 
            prior(normal(0,2), class = "b"))


fit3_q6 <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q6 ~ 1 + condition + (1 | Dyad),
      prior = priors, save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 12000, cores = 4, chains = 4, control = list(adapt_delta = .95),
      file = ".\\brms_fits\\fit3_q6.rds")
toc <- Sys.time()
toc-tic

# print(fit3_q6)
# plot(fit3_q6)

# plot_ordinal(fit_q6)
# plot_ordinal(fit2_q6)
# plot_ordinal(fit3_q6)
# 
# loo_q6 <- loo(fit_q6, moment_match = T)
loo2_q6 <- loo(fit2_q6, moment_match = T)
loo3_q6 <- loo(fit3_q6, moment_match = T)
# 
# loo_compare(loo2_q6, loo3_q6)
# brms::bayes_factor(fit3_q6, fit2_q6)

# loos <- ls()[str_detect(ls(), "loo")]
# 
# for(looi in 1:length(loos)){
#   saveRDS(get(loos[looi]), paste0("C:\\Users\\Sean\\Documents\\LIVELab\\Social_Drumming\\brms_fits\\loo\\", loos[looi],".rds"))
# }
# 
# ###########
# 
# loo_compare(loo2_q1, loo3_q1)
# loo_compare(loo2_q2, loo3_q2)
# loo_compare(loo2_q3, loo3_q3)
# loo_compare(loo2_q4, loo3_q4)
# loo_compare(loo2_q5, loo3_q5)
# loo_compare(loo2_q6, loo3_q6)
########


#########
# priors <- c(prior(normal(-3, 1), coef = 1, class="Intercept"),
#             prior(normal(-2, 1), coef = 2, class="Intercept"),
#             prior(normal(-1, 1), coef = 3, class="Intercept"),
#             prior(normal(0, 1), coef = 4, class="Intercept"),
#             prior(normal(1, 1), coef = 5, class="Intercept"))

# default_prior_model <- brm(data = beh2, family = cumulative(probit), Likert_Q1 ~ 1+ (1|Dyad), sample_prior = "only")

fit2_q1_prior.new <- brm(data = beh2,
                     family = cumulative(probit),
                     Likert_Q1 ~ 1 + (1 | Dyad),
                     prior = priors, sample_prior = "only",
                     )

fit2_q1_prior <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q1 ~ 1 + (1 | Dyad),
      prior = prior_summary(fit2_q1), save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 2000, cores = 4, chains = 4,
      sample_prior = "only",
      control = list(adapt_delta = .99),
      file = ".\\brms_fits\\priors\\fit2_q1.rds")


fit2_q2_prior <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q2 ~ 1 + (1 | Dyad),
      prior = prior_summary(fit2_q2), save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 2000, cores = 4, chains = 4,
      sample_prior = "only",
      control = list(adapt_delta = .99),
      file = ".\\brms_fits\\priors\\fit2_q2.rds")


fit2_q3_prior <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q3 ~ 1 + (1 | Dyad),
      prior = prior_summary(fit2_q3), save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 2000, iter = 4000, cores = 4, chains = 4,
      sample_prior = "only",
      control = list(adapt_delta = .99),
      file = ".\\brms_fits\\priors\\fit2_q3.rds")

fit2_q4_prior <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q4 ~ 1 + (1 | Dyad),
      prior = prior_summary(fit2_q4), save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 2000, cores = 4, chains = 4,
      sample_prior = "only",
      control = list(adapt_delta = .99),
      file = ".\\brms_fits\\priors\\fit2_q4.rds")

fit2_q5_prior <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q5 ~ 1 + (1 | Dyad),
      prior = prior_summary(fit2_q5), save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 2000, cores = 4, chains = 4,
      sample_prior = "only",
      control = list(adapt_delta = .99),
      file = ".\\brms_fits\\priors\\fit2_q5.rds")

fit2_q6_prior <-
  brm(data = beh2,
      family = cumulative(probit),
      Likert_Q6 ~ 1 + (1 | Dyad),
      prior = prior_summary(fit2_q6), save_pars = save_pars(all = TRUE),
      seed = 42, warmup = 1000, iter = 2000, cores = 4, chains = 4,
      sample_prior = "only",
      control = list(adapt_delta = .99),
      file = ".\\brms_fits\\priors\\fit2_q6.rds")







fit2_q1_prior <- brm(data = beh2,
      family = cumulative(probit),
      Likert_Q1 ~ 1 + (1 | Dyad),
      prior = priors, sample_prior = "only")


pp_check(fit2_q1, type = "ecdf_overlay", ndraws = 100)
pp_check(fit2_q1_prior, type = "ecdf_overlay", ndraws = 100)
pp_check(fit2_q1_prior1sd, type = "ecdf_overlay", ndraws = 100)
pp_check(fit2_q1_prior.5sd, type = "ecdf_overlay", ndraws = 100)
pp_check(fit2_q1_prior.new, type = "ecdf_overlay", ndraws = 100)
pp_check(default_prior_model, type = "ecdf_overlay", ndraws = 100)
prior_summary(default_prior_model)


model_list <- c(fit_q1, fit2_q1, fit3_q1,
                fit_q2, fit2_q2, fit3_q2,
                fit_q3, fit2_q3, fit3_q3,
                fit_q4, fit2_q4, fit3_q4,
                fit_q5, fit2_q5, fit3_q5,
                fit_q6, fit2_q6, fit3_q6)