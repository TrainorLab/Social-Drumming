rm(list = ls())
library(tidyverse)
library(psych)


##### change to read from google sheets
#x <- read.csv("C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\Co-operation Drumming.csv")
x <- read.csv("C:\\Users\\mcwee\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Drumming_behavior_1_12_24.csv")

x <- x %>% filter(!Dyad %in% c(102, 104, 220))

###
# do median splits on likert scales by cooperation
## how good was your partner at drumming



psych::describe(x)

x$Coop_Q1 <- ifelse(x$Coop_Q1 == "A", 1, 
                    ifelse(x$Coop_Q1 == "T", 2, NA))

x$Coop_Q2 <- ifelse(x$Coop_Q2 == "A", 1, 
                    ifelse(x$Coop_Q2 == "T", 2, NA))
mean(x$Coop_Q1)
mean(x$Coop_Q2)


t <- mixedCor(x %>% select(Likert_Q1:Coop_Q2))
phi(x %>% select(Coop_Q1, Coop_Q2))


x$condition <- ifelse(x$Dyad < 200, "Alternating", 
                      ifelse(x$Dyad >= 200 & x$Dyad < 300, "Synchrony",
                             ifelse(x$Dyad >= 300, "Alone", NA)))


table(x$condition, x$Coop_Q1)

table(x$condition, x$Coop_Q2)

