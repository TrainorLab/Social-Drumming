rm(list=ls())
library(ISLR)
library(MASS)
library(tidyverse)
library(Rfast)
library(chisq.posthoc.test)
library(RVAideMemoire)
###############

props <- seq(.5,.85,.02)

coop1 <- .93
coop2 <- .84
#coop3 <- .7

n1 <- 40
n2 <- 20
n3 <- 20

z <- c()

for(i in 1:length(props)){
  a1 <- rep("Cooperate", round(n1*coop1, 0))
  a2 <- rep("Defect",  round(n1-(n1*coop1), 0))
  cond <- rep("Sync", n1)
  sync_vec <- cbind(c(a1,a2), cond)
  
  
  a1 <- rep("Cooperate", round(n2*coop2, 0))
  a2 <- rep("Defect",  round(n2-(n2*coop2), 0))
  cond <- rep("Alternating", n2)
  alt_vec <- cbind(c(a1,a2), cond)
  
  a1 <- rep("Cooperate", round(n3*props[i], 0))
  a2 <- rep("Defect",  round(n3-(n3*props[i]), 0))
  cond <- rep("Alone", n3)
  alone_vec <- cbind(c(a1,a2), cond)
  
  sim_data <- data.frame(rbind(sync_vec, alt_vec, alone_vec))
  names(sim_data) <- c("Choice", "Condition")
  
  contingency1 <- t(table(sim_data$Choice, sim_data$Condition))
  prop.table(contingency1, margin = 1)
  mosaicplot(contingency1)
  
  chisq <- chisq.test(contingency1, simulate.p.value = TRUE)
  z[i] <- chisq$p.value
}

plot(props, z, main = "Alone Condition Forecasting: 40 Participants/Group", xlab = "Proportion of Cooperating Participants", ylab = "p-value")
abline(h = .05, col = "red", lty = 2)



chisq.posthoc.test(contingency1)
#####

pre_coop1 <- .75
post_coop1 <- .93

post_coops <- seq(.8,.98,.01)
n1 <- 60 

zz <- data.frame()

for(i in 1:length(post_coops)){
  
  proptest <- prop.test(c(round(n1*pre_coop1, 0), round(n1*post_coops[i], 0)), n = c(n1,n1))
  zz <- rbind(zz, unname(c(proptest$p.value, proptest$statistic)))
  
}

names(zz) <- c("p.val", "statistic")

plot(pre_coops, zz$p.val)
abline(h = .05, col = "red", lty = 2)




















a1_pre <- rep("Cooperate", round(n1*pre_coop1, 0))
a2_pre <- rep("Defect",  round(n1-(n1*pre_coop1), 0))
cond <- rep("Sync", n1)
time <- rep("Pre", n1)
id <- seq(1,n1,1)
pre_sync_vec <- cbind(id, c(a1_pre,a2_pre), cond, time)

a1_post <- rep("Cooperate", round(n1*post_coop1, 0))
a2_post <- rep("Defect",  round(n1-(n1*post_coop1), 0))
time <- rep("Post", n1)
post_sync_vec <- cbind(id, c(a1_post,a2_post), cond, time) 

sync_vec_prepost <- data.frame(cbind(pre_sync_vec, post_sync_vec))
names(sync_vec_prepost) <- c("id", "Choice", "Condition", "prepost", "id2", "Choice2", "Condition2", "prepost2")
#names(sync_vec_prepost) <- c("id", "Choice", "Condition", "prepost")



cont2 <- table(sync_vec_prepost$Choice, sync_vec_prepost$prepost, "")
# cont2 <- table(sync_vec_prepost$Choice, sync_vec_prepost$Choice2)
# 
# 
# mcnemar.test(cont2)





n1 <- 60 
post_coop_vec <- seq(.72,.9, .01)
pre_coop1 <- .7


z2 <- c()

for(i in 1:length(post_coop_vec)){
  a1_pre <- rep("Cooperate", round(n1*pre_coop1, 0))
  a2_pre <- rep("Defect",  round(n1-(n1*pre_coop1), 0))
  cond <- rep("Sync", n1)
  time <- rep("Pre", n1)
  id <- seq(1,n1,1)
  pre_sync_vec <- cbind(id, c(a1_pre,a2_pre), cond, time)
  
  a1_post <- rep("Cooperate", round(n1*post_coop_vec[i], 0))
  a2_post <- rep("Defect",  round(n1-(n1*post_coop_vec[i]), 0))
  time <- rep("Post", n1)
  post_sync_vec <- cbind(id, c(a1_post,a2_post), cond, time) 
  
  sync_vec_prepost <- data.frame(rbind(pre_sync_vec, post_sync_vec))
  names(sync_vec_prepost) <- c("id", "Choice", "Condition", "prepost")
  
  cont2 <- table(sync_vec_prepost$Choice, sync_vec_prepost$prepost)
  
  mcnemar <- mcnemar.test(t(cont2))
  z2[i] <- mcnemar$p.value
}




plot(post_coop_vec,z2)
abline(h = .05, col = "red", lty = 2)






#####
# coop1 <- .90
# coop2 <- .84
# coop3 <- .7
# 
# n1 <- 40
# n2 <- 30
# n3 <- 20
# 
# 
# a1 <- rep("Cooperate", round(n1*coop1, 0))
# a2 <- rep("Defect",  round(n1-(n1*coop1), 0))
# cond <- rep("Sync", n1)
# sync_vec <- cbind(c(a1,a2), cond)
# 
# 
# a1 <- rep("Cooperate", round(n2*coop2, 0))
# a2 <- rep("Defect",  round(n2-(n2*coop2), 0))
# cond <- rep("Alternating", n2)
# alt_vec <- cbind(c(a1,a2), cond)
# 
# a1 <- rep("Cooperate", round(n3*coop3, 0))
# a2 <- rep("Defect",  round(n3-(n3*coop3), 0))
# cond <- rep("Alone", n3)
# alone_vec <- cbind(c(a1,a2), cond)
# 
# sim_data <- data.frame(rbind(sync_vec, alt_vec, alone_vec))
# names(sim_data) <- c("Choice", "Condition")
# 
# contingency1 <- t(table(sim_data$Choice, sim_data$Condition))
# prop.table(contingency1, margin = 1)
# mosaicplot(contingency1)
# 
# chisq <- chisq.test(contingency1)
