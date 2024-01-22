# Social Drumming README
This repository is for cleaning and analyzing data from a between-subjects experiment, in which participants were paired in dyads randomly assigned to one of three drumming conditions: Alone, Synchrony, or Alternating (anti-phase). The dyads completed a behavioral economics stag-hunt game, 4 trials of a synchronization-continuation task, a trust/cooperation questionnaire, and another round of the stag-hunt game. Most of the code base is built to clean the drumming timing data, which was recorded in REAPER, and extracted using a custom script (getnotes.lua). 

The structure of the git is to **store the repository locally on your machine**, while all data are stored on the server at **trainorserv.mcmaster.ca\Sean M\Social_Drumming**. The creator of this repository has trainorserv.mcmaster.ca mapped to the X: drive, which is referenced as such throughout the repository. 

## Data Types and Storage
.rpp files (REAPER_dyad_originals) are the raw MIDI data.

.csv files (REAPER_trial_data) are the trial-level timing data in .csv format (e.g., 102_trial3.csv)

\beh_sync_output\ .rds files are the extracted measurements from each subject, stored as nested lists in R (per trial and per participant) and saved as .rds files

\beh_sync_output\trial_plots\ .png files are visualizations of each step along the cleaning/processing pipeline inside Drumming_Synchrony_Cleaning.R

\.rds files comprise the collated measures from the individual subject (beh_sync_output\) .rds files and the demographic info

Drumming_behavior_1_12_24.csv is the demographic, questionnaire, and stag-hunt outcomes

summary_drumming_trials.xlsx is a human-generated spreadsheet of trials that were successfully cleaned

## Processing Pipeline
1. Drumming_Synchrony_Cleaning.R    
2. aggregate_trials.R
3. aggregate_analysis_viz.R
