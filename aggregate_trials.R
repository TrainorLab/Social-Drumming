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
                        valid_metronome = logical(),
                        mean_met_async_A = numeric(), 
                        n_met_async_A = numeric(), 
                        mean_met_async_B = numeric(), 
                        n_met_async_B = numeric(),
                        pairwise_async = numeric(),
                        onset_async = numeric(),
                        ac1_ITI_A = numeric(),
                        ac1_ITI_B = numeric(),
                        ac1_async = numeric(),
                        detrend_ac1_ITI_A = numeric(),
                        detrend_ac1_ITI_B = numeric(),
                        detrend_ac1_async = numeric(),
                        ITI_var_A = numeric(),
                        ITI_var_B = numeric(),
                        ITI_var_2p = numeric(),
                        
                        stringsAsFactors = FALSE)

# Define the dyads and trials
dyads <- c(101:122, 201:211, 213:222)
trials <- 1:4

# Iterate over each dyad
for (dyad in dyads) {
  # Iterate over each trial within a tryCatch block
  for (trial in trials) {
    tryCatch({
      #print observation 
      print(paste0(dyad, ", ", trial))
      # Read the .Rds file
      x <- readRDS(paste0(output_dir, dyad, "_output.Rds"))
      
      # Create a new row for the data frame
      new_row <- data.frame(dyad = dyad,
                            trial = trial,
                            valid_metronome = x[[trial]][["Valid Metronome"]],
                            mean_met_async_A = x[[trial]][["Mean Metronome Asynchrony: Participant A"]],
                            mean_met_async_A2 = x[[trial]][["Mean Metronome Asynchrony: Participant A (10 Hits)"]],
                            var_met_async_A = x[[trial]][["Variance of Metronome Asynchronies: Participant A"]], 
                            n_met_async_A = x[[trial]][["Metronome Hits: Participant A"]], 
                            mean_met_async_B = x[[trial]][["Mean Metronome Asynchrony: Participant B"]],
                            mean_met_async_B2 = x[[trial]][["Mean Metronome Asynchrony: Participant B (10 Hits)"]],
                            var_met_async_B = x[[trial]][["Variance of Metronome Asynchronies: Participant B"]],
                            n_met_async_B = x[[trial]][["Metronome Hits: Participant B"]],
                            pairwise_async = x[[trial]][["Precision: Pairwise Asynchrony - Continuation Phase"]],
                            onset_async = x[[trial]][["Accuracy: Onset Asynchrony - Continuation Phase"]],
                            ac1_ITI_A = x[[trial]][["Participant A: ITI ACF - Continuation Phase"]][["acf"]][[2]],
                            ac1_ITI_B = x[[trial]][["Participant B: ITI ACF - Continuation Phase"]][["acf"]][[2]],
                            ac1_async = x[[trial]][["Async ACF: Continuation Phase"]][["acf"]][[2]],
                            detrend_ac1_ITI_A = x[[trial]][["Participant A: ITI ACF - Detrended (Cont. Phase)"]][["acf"]][[2]],
                            detrend_ac1_ITI_B = x[[trial]][["Participant B: ITI ACF - Detrended (Cont. Phase)"]][["acf"]][[2]],
                            detrend_ac1_async = x[[trial]][["Detrended Async ACF: Continuation Phase"]][["acf"]][[2]],
                            ITI_var_A = x[[trial]][["Participant A: Tap Variability - Continuation Phase"]],
                            ITI_var_B = x[[trial]][["Participant B: Tap Variability - Continuation Phase"]],
                            ITI_var_2p = x[[trial]][["Dyadic Tap Variability - Continuation Phase"]],
                            ITI_var_A_detrend = x[[trial]][["Participant A: Detrended Tap Variability - Continuation Phase"]],
                            ITI_var_B_detrend = x[[trial]][["Participant B: Detrended Tap Variability - Continuation Phase"]],
                            ITI_var_2p_detrend = x[[trial]][["Dyadic Tap Variability - Detrended Continuation Phase"]],
                            cont_bpm = x[[trial]][["Continuation Phase BPM"]],
                            n_imputed = x[[trial]][["N Imputed"]],
                            clean_hits = x[[trial]][["Clean Hits"]],
                            clean_pct = x[[trial]][["Percent Clean"]],
                            clean2 = x[[trial]][["Clean Hits (Following removed)"]],
                            clean2_pct = x[[trial]][["Percent Clean (Following removed)"]])
      
      # Append the new row to the result data frame
      result_df <- rbind(result_df, new_row)
    }, error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      new_row <- data.frame(dyad = dyad,
                            trial = trial,
                            valid_metronome = NA,
                            mean_met_async_A = NA,
                            mean_met_async_A2 = NA,
                            var_met_async_A = NA,
                            n_met_async_A = NA, 
                            mean_met_async_B = NA,
                            mean_met_async_B2 = NA,
                            var_met_async_B = NA,
                            n_met_async_B = NA,
                            pairwise_async = NA,
                            onset_async = NA,
                            ac1_ITI_A = NA,
                            ac1_ITI_B = NA,
                            ac1_async = NA,
                            detrend_ac1_ITI_A = NA,
                            detrend_ac1_ITI_B = NA,
                            detrend_ac1_async = NA,
                            ITI_var_A = NA,
                            ITI_var_B = NA,
                            ITI_var_2p = NA,
                            ITI_var_A_detrend = NA,
                            ITI_var_B_detrend = NA,
                            ITI_var_2p_detrend = NA,
                            cont_bpm = NA,
                            n_imputed = NA,
                            clean_hits = NA,
                            clean_pct = NA,
                            clean2 = NA,
                            clean2_pct = NA
    )
      result_df <- rbind(result_df, new_row)
    }, warning = function(w) {
      message("A warning occurred: ", conditionMessage(w))
      new_row <- data.frame(dyad = dyad,
                            trial = trial,
                            valid_metronome = NA,
                            mean_met_async_A = NA,
                            mean_met_async_A2 = NA,
                            var_met_async_A = NA,
                            n_met_async_A = NA, 
                            mean_met_async_B = NA,
                            mean_met_async_B2 = NA,
                            var_met_async_B = NA,
                            n_met_async_B = NA,
                            pairwise_async = NA,
                            onset_async = NA,
                            ac1_ITI_A = NA,
                            ac1_ITI_B = NA,
                            ac1_async = NA,
                            detrend_ac1_ITI_A = NA,
                            detrend_ac1_ITI_B = NA,
                            detrend_ac1_async = NA,
                            ITI_var_A = NA,
                            ITI_var_B = NA,
                            ITI_var_2p = NA,
                            ITI_var_A_detrend = NA,
                            ITI_var_B_detrend = NA,
                            ITI_var_2p_detrend = NA,
                            cont_bpm = NA,
                            n_imputed = NA,
                            clean_hits = NA,
                            clean_pct = NA,
                            clean2 = NA,
                            clean2_pct = NA)
      
      result_df <- rbind(result_df, new_row)
    })
  }
}

# Reset row names of the result data frame
rownames(result_df) <- NULL

beh <- read.csv("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\Co-operation Drumming.csv")
beh$Dyad <- as.numeric(beh$Dyad)
beh <- beh %>%
  mutate(condition = ifelse(Dyad < 200, "Alternating", 
                            ifelse(Dyad > 200 & Dyad <300, "Synchrony", 
                                   ifelse(Dyad > 300, "Alone", NA)))) %>%
  mutate(Exclude = Dyad %in% c(102, 104, 220))

trial_summary <- readxl::read_xlsx("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\summary_drumming_trials.xlsx")

df <- left_join(beh, result_df, by = c("Dyad" = "dyad"))
df <- left_join(df, trial_summary, by = c("Dyad" = "id", "trial"))

df <- df %>% 
  filter(clean == "Y") %>%
  mutate(mean_met_async = ifelse(ID == "A", mean_met_async_A, mean_met_async_B)) %>%
  mutate(mean_met_async2 = ifelse(ID == "A", mean_met_async_A2, mean_met_async_B2)) %>%
  mutate(var_met_async = ifelse(ID == "A", var_met_async_A, var_met_async_B)) %>%
  mutate(desync_events = as.numeric(desync_events)) %>%
  mutate(ac1_ITI = ifelse(ID == "A", ac1_ITI_A, ac1_ITI_B)) %>%
  mutate(detrend_ac1_ITI = ifelse(ID == "A", detrend_ac1_ITI_A, detrend_ac1_ITI_B)) %>%
  mutate(ITI_var_1p = ifelse(ID == "A", ITI_var_A, ITI_var_B)) %>%
  mutate(ITI_var_1p_detrend = ifelse(ID == "A", ITI_var_A_detrend, ITI_var_B_detrend)) %>%
  mutate(log_ITI_var_1p = log(ITI_var_1p)) %>%
  mutate(log_ITI_var_2p = log(ITI_var_2p)) %>%
  mutate(log_ITI_var_1p_detrend = log(ITI_var_1p_detrend)) %>%
  mutate(log_ITI_var_2p_detrend = log(ITI_var_2p_detrend)) %>%
  group_by(Dyad) %>%
  mutate(ac1_detrend_diff = ifelse(ID == "B", abs(detrend_ac1_ITI[ID == "A"] - detrend_ac1_ITI), abs(detrend_ac1_ITI[ID == "B"] - detrend_ac1_ITI))) %>%
  ungroup() %>%
  mutate(ac1_detrend_diff_cat = ifelse(ac1_detrend_diff > median(ac1_detrend_diff), "Large Adaptation Difference", "Small Adaptation Difference")) %>%
  select(-ITI_var_A, -ITI_var_B, -ac1_ITI_A, -ac1_ITI_B, -mean_met_async_A, -mean_met_async_B, -var_met_async_A, -var_met_async_B)

trial_df <- df

#####
# df_pivoted <- df %>%
#   group_by(Dyad, trial) %>%
#   pivot_wider(names_from = ID, values_from = detrend_ac1_ITI, names_prefix = "ITI_AC1_", names_glue = "ITI_AC1_{ID}") %>%
#   ungroup()
#####
summary_df <- df %>%
  group_by(Dyad, ID) %>%
  summarize(
    n_clean_trials = n(),
    mean_mean_met_async = mean(mean_met_async),
    mean_mean_met_async2 = mean(mean_met_async2),
    mean_var_met_async = mean(var_met_async),
    desyncs = sum(desync_events, na.rm = T),
    pairwise_async_mean = mean(pairwise_async),
    onset_async_mean = mean(onset_async),
    ac1_async_mean = mean(ac1_async),
    detrend_ac1_async_mean = mean(detrend_ac1_async),
    ac1_ITI_mean = mean(ac1_ITI),
    detrend_ac1_ITI_mean = mean(detrend_ac1_ITI),
    cont_bpm = mean(cont_bpm),
    n_imputed = mean(n_imputed),
    clean_hits = mean(clean_hits),
    clean_hits2 = mean(clean2),
    ITI_var_1p = mean(ITI_var_1p),
    ITI_var_2p = mean(ITI_var_2p),
    log_ITI_var_1p = mean(log_ITI_var_1p),
    log_ITI_var_2p = mean(log_ITI_var_2p),
    ITI_var_1p_detrend = mean(ITI_var_1p_detrend),
    ITI_var_2p_detrend = mean(ITI_var_2p_detrend),
    log_ITI_var_1p_detrend = mean(log_ITI_var_1p_detrend),
    log_ITI_var_2p_detrend = mean(log_ITI_var_2p_detrend),
    n_included_trials = n(),
    ac1_detrend_diff = mean(ac1_detrend_diff)
  )


avg_df <- full_join(summary_df, beh)
avg_df <- avg_df %>%
  filter(n_clean_trials >= 1) %>% ungroup() %>%
  mutate(ac1_detrend_diff_cat = ifelse(ac1_detrend_diff > median(ac1_detrend_diff), "Large Adaptation Difference", "Small Adaptation Difference")) %>%
  mutate(ac1_detrend_high_low = ifelse(detrend_ac1_ITI_mean < median(detrend_ac1_ITI_mean), "High Adaptation", "Low Adaptation")) %>%
  group_by(Dyad)
  


avg_df$Dyad <- factor(avg_df$Dyad)
avg_df$ID <- factor(avg_df$ID)


avg_df <- avg_df %>%
  select(condition, ID, Dyad, mean_mean_met_async, mean_mean_met_async2, mean_var_met_async, ac1_ITI_mean, detrend_ac1_ITI_mean, ITI_var_1p:ac1_detrend_high_low, desyncs, clean_hits, clean_hits2, cont_bpm, Likert_Q1:Exclude)

write_rds(beh, "C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\beh_df.rds")
write_rds(avg_df, "C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_avgs_df.rds")
write_rds(trial_df, "C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\trial_df.rds")
