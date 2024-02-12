library(readr)
library(psych)
library(tidyverse)
library(rethinking)
library(bayestestR)
library(BayesFactor)
library(lme4)
library(brms)
library(rmcorr)
library(rstan)
library(rstanarm)
rm(list = ls())
trial_df <- read_rds("X:\\Sean M\\Social_Drumming\\trial_df.rds")
avg_df <- read_rds("X:\\Sean M\\Social_Drumming\\trial_avgs_df.rds")
beh <- read_rds("X:\\Sean M\\Social_Drumming\\beh_df.rds")

beh <- beh %>% filter(Exclude == FALSE)

#averaging within dyad for statistical simplicity
beh_dyad_avg <- beh %>%
  group_by(Dyad) %>%
  mutate(dyad_Likert_Q1 = mean(Likert_Q1),
         dyad_Likert_Q2 = mean(Likert_Q2),
         dyad_Likert_Q3 = mean(Likert_Q3),
         dyad_Likert_Q4 = mean(Likert_Q4),
         dyad_Likert_Q5 = mean(Likert_Q5),
         dyad_Likert_Q6 = mean(Likert_Q6),
         condition = as.factor(condition)) %>%
  filter(ID == "A") %>%
  select(Dyad, condition, dyad_Likert_Q1:dyad_Likert_Q6)

tbl1 <- beh_dyad_avg %>% 
  group_by(condition) %>%
  summarize(mean_Likert1 = mean(dyad_Likert_Q1),
            mean_Likert2 = mean(dyad_Likert_Q2),
            mean_Likert3 = mean(dyad_Likert_Q3),
            mean_Likert4 = mean(dyad_Likert_Q4),
            mean_Likert5 = mean(dyad_Likert_Q5),
            mean_Likert6 = mean(dyad_Likert_Q6)) %>%
  mutate(across(2:7, round, 2))



tbl1_SD <- beh_dyad_avg %>% 
  group_by(condition) %>%
  summarize(sd_Likert1 = sd(dyad_Likert_Q1),
            sd_Likert2 = sd(dyad_Likert_Q2),
            sd_Likert3 = sd(dyad_Likert_Q3),
            sd_Likert4 = sd(dyad_Likert_Q4),
            sd_Likert5 = sd(dyad_Likert_Q5),
            sd_Likert6 = sd(dyad_Likert_Q6)) %>%
  mutate(across(2:7, round, 2))

zz <- data.frame()
i = 3
j = 1
for(i in 1:3){
  for(j in 1:6){
    zz[i,j] <- paste0(sprintf("%.2f",tbl1[i,j+1]), " (", sprintf("%.2f",tbl1_SD[i,j+1]), ")")
  }
}

zz <- c(tbl1[,1], zz)


names(zz) <- c("Condition", "Unit", "Same Team", "Trust Before", "Similar", "Cooperated", "Happy")

print(nice_table(zz), preview = "docx")


#apa.1way.table(iv = condition, dv = dyad_Likert_Q1, data = beh_dyad_avg, filename = "table_test.doc")




#####
beh_contingency <- beh %>% select(Coop_Q1, Coop_Q2, condition) %>%
  pivot_longer(cols = c("Coop_Q1", "Coop_Q2"), names_to = "timepoint", values_to = "Coop") %>%
  mutate(time = case_when(timepoint == "Coop_Q1" ~ "Pre",
                          timepoint == "Coop_Q2" ~ "Post"))

table(beh_contingency$time, beh_contingency$condition, beh_contingency$Coop)


##### We're going to run into non-independence issues, but let's first look to see how correlated the dyads' scores are
##### 
wide_beh_vars <- beh %>%
  group_by(Dyad) %>% 
  dplyr::select(Dyad, ID, Likert_Q1:Coop_Q2) %>%
  pivot_wider(names_from = ID, values_from = c(Likert_Q1:Coop_Q2))
wide_beh_cors <- cor(wide_beh_vars %>% ungroup() %>% dplyr::select(-Dyad, -starts_with("Coop")))

# beh$fullID <- paste(as.character(beh$Dyad), beh$ID)

# rm_cormat_likert <- rmcorr::rmcorr_mat(dataset = beh,
                                     # participant = "Dyad", variables = c(names(beh)[str_detect(names(beh),"Like")], "Coop_Q1", "Coop_Q2"))

# temp <- beh %>% mutate(across(starts_with("Coop"), ~ case_match(., "T" ~ 1, "A" ~ 0))) 
# temp <- beh %>% mutate(across(starts_with("Coop"), ~ case_match(., "T" ~ 5, "A" ~ -2))) 


rm_cormat_likert <- rmcorr::rmcorr_mat(dataset = beh,
                                       participant = "Dyad", variables = c(names(beh)[str_detect(names(beh),"Like")]))

rmcorr("Likert_Q1", "Likert_Q2", participant = "Dyad", dataset = beh)

scale = 3
png("C:\\Users\\mcwee\\Documents\\LIVELab\\NeuroMusic23\\plots\\rm_cor.png", width = 800*scale, height = 480*scale, res = 72*scale)
corPlot(rm_cormat_likert$matrix, labels = c("Unit", "Same Team", "Trust Before", "Similar", "Cooperated", "Happy"),
        main = "Correlations among Likert Items")
dev.off()

corPlot(wide_beh_cors)

wide_beh_vars

beh$Likert_Q1 <- as.numeric(beh$Likert_Q1)
beh$Likert_Q2 <- as.numeric(beh$Likert_Q2)
beh$Likert_Q3 <- as.numeric(beh$Likert_Q3)
beh$Likert_Q4 <- as.numeric(beh$Likert_Q4)
beh$Likert_Q5 <- as.numeric(beh$Likert_Q5)
beh$Likert_Q6 <- as.numeric(beh$Likert_Q6)
beh$Coop_Q1 <- ifelse(beh$Coop_Q1 == "A", 0, 1)
beh$Coop_Q2 <- ifelse(beh$Coop_Q2 == "A", 0, 1)
beh$Dyad <- as.factor(beh$Dyad)
beh$condition <- as.factor(beh$condition)

#beh$Likert_Q1_std <- (beh$Likert_Q1 - mean(beh$Likert_Q1))/sd(beh$Likert_Q1)

df_pp <- beh %>% select(Likert_Q1:Coop_Q2)
pairs.panels(df_pp)

psych::cor2(beh$Likert_Q1, beh$Coop_Q2)

rmcorr(participant = factor(Dyad), measure1 = Likert_Q1, measure2 = Likert_Q6, dataset = beh)
rmcormat_likert_coop <- rmcorr_mat(participant = factor(Dyad), variables = c("Likert_Q1", "Likert_Q2", "Likert_Q3", "Likert_Q4", "Likert_Q5", "Likert_Q6", "Coop_Q1", "Coop_Q2"), dataset = beh)

scale = 3.5
png("C:\\Users\\mcwee\\Documents\\LIVELab\\NeuroMusic23\\plots\\rm_cor.png", width = 800*scale, height = 480*scale, res = 72*scale)
psych::corPlot(rmcormat_likert_coop[["matrix"]])
dev.off()
#long format for plotting
#####
beh_long <- beh %>% 
  pivot_longer(names_to = "Q", values_to = "Likert_Score", cols = c("Likert_Q1", "Likert_Q2", "Likert_Q3", "Likert_Q4", "Likert_Q5", "Likert_Q6"))


#grouped for plotting mean/se
SE <- function(x){
  sd(x)/sqrt(length(x))
}

mean_se_plot <- beh_long %>% 
  ungroup() %>% 
  select(Likert_Score, Q, condition) %>% 
  group_by(condition, Q) %>%
  summarise(mean = mean(Likert_Score),
         se = SE(Likert_Score),
         ci.u = mean + 1.96*se,
         ci.l = mean - 1.96*se) 
  
mean_se_plot <- mean_se_plot %>% ungroup() %>%
  mutate(Q_lang = case_when((Q == "Likert_Q1") ~ "Unit",
                            (Q == "Likert_Q2") ~ "Same Team",
                            (Q == "Likert_Q3") ~ "Trust Before",
                            (Q == "Likert_Q4") ~ "Similar",
                            (Q == "Likert_Q5") ~ "Cooperated",
                            (Q == "Likert_Q6") ~ "Happy")) %>%
  mutate(Q_lang = factor(Q_lang, levels = c("Unit", "Same Team", "Trust Before", "Similar", "Cooperated", "Happy")))




#####
ggplot(data = beh_long, aes(x = condition, y = Likert_Score, fill = condition)) +
  geom_violin() + 
#  geom_point() +
  facet_wrap(~Q)

scale = 3
png(filename = "C:\\Users\\mcwee\\Documents\\LIVELab\\NeuroMusic23\\Likert_scores_by_condition.png", height = 480*scale, width =  960*scale, res = 72*scale)
ggplot(data = mean_se_plot, aes(x = Q_lang, y = mean, fill = condition)) +
  geom_bar(position = position_dodge(), stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = ci.l, ymax = ci.u), width = .2, position = position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2", name = "Condition") +
  labs(x = "Likert Question", 
       y = "Score", 
       title = "Likert Scores by Condition") + 
  theme_classic() +
  ggplot2::theme(axis.title = element_text(size = 20), 
                 axis.text = element_text(size = 18),
                 legend.title = element_text(size = 16), 
                 legend.text = element_text(size = 14),
                 title = element_text(size = 24))
dev.off()




#####

##### frequentist ANOVAs on averaged dyad data
#####
dyad.aov1 <- aov(dyad_Likert_Q1 ~ condition, data = beh_dyad_avg)
summary(dyad.aov1)
dyad.post1 <- multcomp::glht(dyad.aov1, linfct = multcomp::mcp(condition = "Tukey"))
summary(dyad.post1)

dyad.aov2 <- aov(dyad_Likert_Q2 ~ condition, data = beh_dyad_avg)
summary(dyad.aov2)
dyad.post2 <- multcomp::glht(dyad.aov2, linfct = multcomp::mcp(condition = "Tukey"))
summary(dyad.post2)

dyad.aov3 <- aov(dyad_Likert_Q3 ~ condition, data = beh_dyad_avg)
summary(dyad.aov3)
dyad.post3 <- multcomp::glht(dyad.aov3, linfct = multcomp::mcp(condition = "Tukey"))
summary(dyad.post3)

dyad.aov4 <- aov(dyad_Likert_Q4 ~ condition, data = beh_dyad_avg)
summary(dyad.aov4)
dyad.post4 <- multcomp::glht(dyad.aov4, linfct = multcomp::mcp(condition = "Tukey"))
summary(dyad.post4)

dyad.aov5 <- aov(dyad_Likert_Q5 ~ condition, data = beh_dyad_avg)
summary(dyad.aov5)
dyad.post5 <- multcomp::glht(dyad.aov5, linfct = multcomp::mcp(condition = "Tukey"))
summary(dyad.post5)

dyad.aov6 <- aov(dyad_Likert_Q6 ~ condition, data = beh_dyad_avg)
summary(dyad.aov6)
dyad.post6 <- multcomp::glht(dyad.aov6, linfct = multcomp::mcp(condition = "Tukey"))
summary(dyad.post6)
#####


##### Bayesian t-tests collapsing across dyads
#####
# q1_tt_bayes <- ttestBF(formula = dyad_Likert_Q1 ~ condition, data = beh_dyad_avg %>% filter(condition != "Alone"))
# q2_tt_bayes <- ttestBF(formula = dyad_Likert_Q2 ~ condition, data = beh_dyad_avg %>% filter(condition != "Alone"))
# q3_tt_bayes <- ttestBF(formula = dyad_Likert_Q3 ~ condition, data = beh_dyad_avg %>% filter(condition != "Alone"))
# q4_tt_bayes <- ttestBF(formula = dyad_Likert_Q4 ~ condition, data = beh_dyad_avg %>% filter(condition != "Alone"))
# q5_tt_bayes <- ttestBF(formula = dyad_Likert_Q5 ~ condition, data = beh_dyad_avg %>% filter(condition != "Alone"))
# q6_tt_bayes <- ttestBF(formula = dyad_Likert_Q6 ~ condition, data = beh_dyad_avg %>% filter(condition != "Alone"))
# 
# 1/q1_tt_bayes
# 1/q2_tt_bayes
# 1/q3_tt_bayes
# 1/q4_tt_bayes
# 1/q5_tt_bayes
# 1/q6_tt_bayes


#####


##### Mixed Effects Models to handle non-independence assumptions
#####
null.lme1 <- lmer(data = beh, Likert_Q1 ~ (1 | Dyad))
lme1 <- lmer(data = beh, Likert_Q1 ~ condition + (1 | Dyad))
summary(lme1)

null.lme2 <- lmer(data = beh, Likert_Q2 ~ (1 | Dyad))
lme2 <- lmer(data = beh, Likert_Q2 ~ condition + (1 | Dyad))
summary(lme2)

null.lme3 <- lmer(data = beh, Likert_Q3 ~ (1 | Dyad))
lme3 <- lmer(data = beh, Likert_Q3 ~ condition + (1 | Dyad))
summary(lme3)

null.lme4 <- lmer(data = beh, Likert_Q4 ~ (1 | Dyad))
lme4 <- lmer(data = beh, Likert_Q4 ~ condition + (1 | Dyad))
summary(lme4)

null.lme5 <- lmer(data = beh, Likert_Q5 ~ (1 | Dyad))
lme5 <- lmer(data = beh, Likert_Q5 ~ condition + (1 | Dyad))
summary(lme5)

null.lme6 <- lmer(data = beh, Likert_Q6 ~ (1 | Dyad))
lme6 <- lmer(data = beh, Likert_Q6 ~ condition + (1 | Dyad))
summary(lme6)
#####
##### Model Comparison for lme 
#####
anova(null.lme1, lme1)
anova(null.lme2, lme2)
anova(null.lme3, lme3)
anova(null.lme4, lme4)
anova(null.lme5, lme5)
anova(null.lme6, lme6)
#####
# Top paragraph of "Statistical Approach"
### Messed around with flipping the IV/DV starting at line 314 (Flipped IV/DV)
coop1_lme1 <- lmer(data = beh, Likert_Q1 ~ as.factor(Coop_Q1) + (1 | Dyad))
coop2_lme1 <- lmer(data = beh, Likert_Q1 ~ as.factor(Coop_Q2) + (1 | Dyad))
coop1_lme2 <- lmer(data = beh, Likert_Q2 ~ as.factor(Coop_Q1) + (1 | Dyad))
coop2_lme2 <- lmer(data = beh, Likert_Q2 ~ as.factor(Coop_Q2) + (1 | Dyad))
coop1_lme3 <- lmer(data = beh, Likert_Q3 ~ as.factor(Coop_Q1) + (1 | Dyad))
coop2_lme3 <- lmer(data = beh, Likert_Q3 ~ as.factor(Coop_Q2) + (1 | Dyad))
coop1_lme4 <- lmer(data = beh, Likert_Q4 ~ as.factor(Coop_Q1) + (1 | Dyad))
coop2_lme4 <- lmer(data = beh, Likert_Q4 ~ as.factor(Coop_Q2) + (1 | Dyad))
coop1_lme5 <- lmer(data = beh, Likert_Q5 ~ as.factor(Coop_Q1) + (1 | Dyad))
coop2_lme5 <- lmer(data = beh, Likert_Q5 ~ as.factor(Coop_Q2) + (1 | Dyad))
coop1_lme6 <- lmer(data = beh, Likert_Q6 ~ as.factor(Coop_Q1) + (1 | Dyad))
coop2_lme6 <- lmer(data = beh, Likert_Q6 ~ as.factor(Coop_Q2) + (1 | Dyad))

anova(null.lme1, coop1_lme1)
anova(null.lme1, coop2_lme1)

anova(null.lme2, coop1_lme2)
anova(null.lme2, coop2_lme2)

anova(null.lme3, coop1_lme3)
anova(null.lme3, coop2_lme3)

anova(null.lme4, coop1_lme4)
anova(null.lme4, coop2_lme4)

anova(null.lme5, coop1_lme5)
anova(null.lme5, coop2_lme5)

anova(null.lme6, coop1_lme6)
anova(null.lme6, coop2_lme6)




##### Mixed Effects Models to handle non-independence assumptions
#####
null.lme1 <- lmer(data = beh, Likert_Q1 ~ (1 | Dyad))
lme1 <- lmer(data = beh, Likert_Q1 ~ condition + (1 | Dyad))
summary(lme1)

null.lme2 <- lmer(data = beh, Likert_Q2 ~ (1 | Dyad))
lme2 <- lmer(data = beh, Likert_Q2 ~ condition + (1 | Dyad))
summary(lme2)

null.lme3 <- lmer(data = beh, Likert_Q3 ~ (1 | Dyad))
lme3 <- lmer(data = beh, Likert_Q3 ~ condition + (1 | Dyad))
summary(lme3)

null.lme4 <- lmer(data = beh, Likert_Q4 ~ (1 | Dyad))
lme4 <- lmer(data = beh, Likert_Q4 ~ condition + (1 | Dyad))
summary(lme4)

null.lme5 <- lmer(data = beh, Likert_Q5 ~ (1 | Dyad))
lme5 <- lmer(data = beh, Likert_Q5 ~ condition + (1 | Dyad))
summary(lme5)

null.lme6 <- lmer(data = beh, Likert_Q6 ~ (1 | Dyad))
lme6 <- lmer(data = beh, Likert_Q6 ~ condition + (1 | Dyad))
summary(lme6)
#####
##### Model Comparison for lme 
#####
anova(null.lme1, lme1)
anova(null.lme2, lme2)
anova(null.lme3, lme3)
anova(null.lme4, lme4)
anova(null.lme5, lme5)
anova(null.lme6, lme6)
#####
# Flipped IV/DV
coop1_lme1 <- lmer(data = beh, Coop_Q1 ~ Likert_Q1 + (1 | Dyad))
coop1_lme2 <- lmer(data = beh, Coop_Q1 ~ Likert_Q2 + (1 | Dyad))
coop1_lme3 <- lmer(data = beh, Coop_Q1 ~ Likert_Q3 + (1 | Dyad))
coop1_lme4 <- lmer(data = beh, Coop_Q1 ~ Likert_Q4 + (1 | Dyad))
coop1_lme5 <- lmer(data = beh, Coop_Q1 ~ Likert_Q5 + (1 | Dyad))
coop1_lme6 <- lmer(data = beh, Coop_Q1 ~ Likert_Q6 + (1 | Dyad))

coop2_lme1 <- lmer(data = beh, Coop_Q2 ~ Likert_Q1 + (1 | Dyad))
coop2_lme2 <- lmer(data = beh, Coop_Q2 ~ Likert_Q2 + (1 | Dyad))
coop2_lme3 <- lmer(data = beh, Coop_Q2 ~ Likert_Q3 + (1 | Dyad))
coop2_lme4 <- lmer(data = beh, Coop_Q2 ~ Likert_Q4 + (1 | Dyad))
coop2_lme5 <- lmer(data = beh, Coop_Q2 ~ Likert_Q5 + (1 | Dyad))
coop2_lme6 <- lmer(data = beh, Coop_Q2 ~ Likert_Q6 + (1 | Dyad))


null.coop1 <- lmer(data = beh, Coop_Q1 ~ (1 | Dyad))

null.coop2 <- lmer(data = beh, Coop_Q2 ~ (1 | Dyad))


anova(null.coop1, coop1_lme1)
anova(null.coop2, coop2_lme1)

anova(null.coop1, coop1_lme2)
anova(null.coop2, coop2_lme2)

anova(null.coop1, coop1_lme3)
anova(null.coop2, coop2_lme3)

anova(null.coop1, coop1_lme4)
anova(null.coop2, coop2_lme4)

anova(null.coop1, coop1_lme5)
anova(null.coop2, coop2_lme5)

anova(null.coop1, coop1_lme6)
anova(null.coop2, coop2_lme6)















# Bayes Time

beh2 <- beh %>% filter(condition != "Alone")

full_formula <- brmsformula(Likert_Q1 ~ 1 + condition + (1 | Dyad))
null_formula <- brmsformula(Likert_Q1 ~ 1 + (1 | Dyad))

priors <- c(prior(normal(5, 1), class="Intercept"),
            prior(normal(0,.5), class = "b"),
            prior(exponential(1), class = "sigma"))

full_model1 <- brm(full_formula, data = beh2, priors = priors, warmup = 500, chains = 4, cores = 4)
null_model1 <- brm(null_formula, data = beh2, warmup = 50, iter = 200, chains = 4, cores = 4)





stan_q1 <- stan_lmer(Likert_Q1 ~ condition + (1 | Dyad),
                     data = beh2, chains = 4, cores = 4)

stan_q1_null <- stan_lmer(Likert_Q1 ~ (1 | Dyad),
          data = beh2, chains = 4, cores = 4)


loo_stan_q1 <- loo(stan_q1, k_threshold = 0.7)
loo_stan_q1_null <- loo(stan_q1_null, k_threshold = 0.7)



bayes_factor <- loo::loo_compare(loo_stan_q1, loo_stan_q1_null)

exp(data.frame(bayes_factor)$elpd_diff[2])










# # likelihood function
# reg_ll = function(X, y, beta, sigma){
#   sum(dnorm(y, mean=X%*%beta, sd=sigma, log=T))
# }
# 
# true_beta = c(2,5)
# true_sigma = 1
# 
# # comparison values
# other_beta = c(0,3)
# other_sigma = 2
# 
# # sample size
# N = 1000
# 
# # data generation
# X = cbind(1, runif(N))
# y = X %*% true_beta + rnorm(N, sd=true_sigma)
# 
# # calculate likelihooods
# reg_ll(X, y, beta=true_beta, sigma=true_sigma)    # more likely
# 
# reg_ll(X, y, beta=other_beta, sigma=other_sigma)  # less likely
# 
# logLik(lm(y~., data=data.frame(X[,-1])))          # actual log likelihood












#####

#new_row <- data.frame(condition = "Synchrony", Likert_Q1 = 5, Likert_Q2 = 5, Likert_Q3 = 5, Likert_Q4 = 5, Likert_Q5 = 5, Likert_Q6 = 5)

# beh2 <- beh2 %>%
#   add_row(new_row)



