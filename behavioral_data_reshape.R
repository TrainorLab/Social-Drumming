rm(list = ls())
library(tidyverse)
library(psych)


##### change to read from google sheets
x <- read.csv("C:\\Users\\mcwee\\Downloads\\Cooperation_Drumming_2_2_23.csv")

x <- x %>% filter(!Dyad %in% c(102, 104))

x %>% tidyverse::select(Likert_Q1)

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


psych::pairs.panels(x %>% select(Likert_Q1, Likert_Q6))

x$condition <- ifelse(x$Dyad < 200, "Alternating", 
                      ifelse(x$Dyad >= 200 & x$Dyad < 300, "Synchrony",
                             ifelse(x$Dyad >= 300, "Alone", NA)))

x %>% group_by(condition) %>%
  summarise(mean_coop1 = mean(Coop_Q1)) 

x %>% group_by(condition) %>%
  summarise(mean_coop2 = mean(Coop_Q2))

x %>% group_by(condition) %>%
  summarise(Likert1 = mean(Likert_Q1))

