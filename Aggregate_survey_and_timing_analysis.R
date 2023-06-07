rm(list=ls())
user <- "SM"

if(user == "SM"){
  output_dir <- "C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_sync_output\\"
  setwd("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\")
} else if(user == "AL"){
  setwd("~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation")
  data_dir <- "~/McMaster/Third Year/PNB 3QQ3/Drumming and Cooperation/Data/"
} else if(user == "YAS"){
  data_dir <- ""
}

library(tidyverse)
library(rlang)
library(zoo)
library(ggplot2)


# Create an empty data frame to store the results
# Create an empty data frame to store the results
result_df <- data.frame(dyad = numeric(),
                        trial = numeric(),
                        pairwise_async = numeric(),
                        onset_async = numeric(),
                        ac1_ITI_A = numeric(),
                        ac1_ITI_B = numeric(),
                        ac1_async = numeric(),
                        detrend_ac1_ITI_A = numeric(),
                        detrend_ac1_ITI_B = numeric(),
                        detrend_ac1_async = numeric(),
                        stringsAsFactors = FALSE)

# Define the dyads and trials
dyads <- c(101:119, 201:211)
trials <- 1:4

# Iterate over each dyad
for (dyad in dyads) {
  # Iterate over each trial within a tryCatch block
  for (trial in trials) {
    tryCatch({
      # Read the .Rds file
      x <- readRDS(paste0(output_dir, dyad, "_output.Rds"))
      

      # Create a new row for the data frame
      new_row <- data.frame(dyad = dyad,
                            trial = trial,
                            pairwise_async = x[[trial]][["Precision: Pairwise Asynchrony - Continuation Phase"]],
                            onset_async = x[[trial]][["Accuracy: Onset Asynchrony - Continuation Phase"]],
                            ac1_ITI_A = x[[trial]][["Participant A: ITI ACF - Continuation Phase"]][["acf"]][[2]],
                            ac1_ITI_B = x[[trial]][["Participant B: ITI ACF - Continuation Phase"]][["acf"]][[2]],
                            ac1_async = x[[trial]][["Async ACF: Continuation Phase"]][["acf"]][[2]],
                            detrend_ac1_ITI_A = x[[trial]][["Participant A: ITI ACF - Detrended (Cont. Phase)"]][["acf"]][[2]],
                            detrend_ac1_ITI_B = x[[trial]][["Participant B: ITI ACF - Detrended (Cont. Phase)"]][["acf"]][[2]],
                            detrend_ac1_async = x[[trial]][["Detrended Async ACF: Continuation Phase"]][["acf"]][[2]])
      
      # Append the new row to the result data frame
      result_df <- rbind(result_df, new_row)
    }, error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      new_row <- data.frame(dyad = dyad,
                            trial = trial,
                            pairwise_async = NA,
                            onset_async = NA,
                            ac1_ITI_A = NA,
                            ac1_ITI_B = NA,
                            ac1_async = NA,
                            detrend_ac1_ITI_A = NA,
                            detrend_ac1_ITI_B = NA,
                            detrend_ac1_async = NA)
      result_df <- rbind(result_df, new_row)
    }, warning = function(w) {
      message("A warning occurred: ", conditionMessage(w))
      new_row <- data.frame(dyad = dyad,
                            trial = trial,
                            pairwise_async = NA,
                            onset_async = NA,
                            ac1_ITI_A = NA,
                            ac1_ITI_B = NA,
                            ac1_async = NA,
                            detrend_ac1_ITI_A = NA,
                            detrend_ac1_ITI_B = NA,
                            detrend_ac1_async = NA)
      result_df <- rbind(result_df, new_row)
    })
  }
}

# Reset row names of the result data frame
rownames(result_df) <- NULL

psych::describe(result_df)


beh <- read.csv("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\Co-operation Drumming.csv")
beh <- beh %>% filter(Dyad < 300)

trial_summary <- readxl::read_xlsx("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\summary_drumming_trials.xlsx")

df <- left_join(beh, result_df, by = c("Dyad" = "dyad"))
df <- left_join(df, trial_summary, by = c("Dyad" = "id", "trial"))

df <- df %>%
  filter(clean == "Y") %>%
  mutate(desync_events = as.numeric(desync_events))

df <- df %>%
  mutate(ac1_ITI = ifelse(ID == "A", ac1_ITI_A, ac1_ITI_B)) %>%
  select(-ac1_ITI_A, -ac1_ITI_B) %>%
  mutate(detrend_ac1_ITI = ifelse(ID == "A", detrend_ac1_ITI_A, detrend_ac1_ITI_B)) 


summary_df <- df %>%
  group_by(Dyad, ID) %>%
  summarize(
    n_clean_trials = n(),
    desyncs = sum(desync_events, na.rm = T),
    pairwise_async_mean = mean(pairwise_async),
    onset_async_mean = mean(onset_async),
    ac1_async_mean = mean(ac1_async),
    detrend_ac1_async_mean = mean(detrend_ac1_async),
    ac1_ITI_mean = mean(ac1_ITI),
    detrend_ac1_ITI_mean = mean(detrend_ac1_ITI)
  )


big_df <- full_join(summary_df, beh)
big_df <- big_df %>%
  filter(n_clean_trials >= 1)
big_df$Dyad <- factor(big_df$Dyad)
big_df$ID <- factor(big_df$ID)



big_df <- big_df %>%
  select(ID, Dyad,  ac1_ITI_mean, detrend_ac1_ITI_mean,  Likert_Q1:Likert_Q6, Coop_Q1:Coop_Q2)

pairs.panels(big_df)

library(lme4)

model <- lmer(Likert_Q2 ~ ac1_ITI_mean + (1 | Dyad), data = big_df)

library(sjPlot)

# Assuming your model object is called "model"
plot_model(model, type = "pred", terms = c("ac1_ITI_mean"), plot.ci = TRUE)

# Assuming your model object is called "model"
residuals <- residuals(model)

# Histogram
hist(residuals, main = "Histogram of Residuals")

# Q-Q plot
qqnorm(residuals)
qqline(residuals)


# Assuming your model object is called "model"
predicted <- predict(model)
plot(residuals ~ predicted, main = "Residuals vs. Predicted Values")

# Or, if you have a single predictor variable, ac1_ITI_mean
plot(residuals ~ big_df$ac1_ITI_mean, main = "Residuals vs. ac1_ITI_mean")


