library(tidyverse)
library(pwr)

#https://www.socscistatistics.com/tests/chisquare2/default2.aspx
chi2 <- 6.667
n = 60
df = 1
V = sqrt(chi2/(n*df))


w_real <- sqrt(4.49/27)


w <- seq(.2, .5, .05)
N  <- seq(30, 120, 3)


pwr.chisq.test(w = w, df = 2, sig.level = .05, N = 90)

x <- pwr.chisq.test(w = w, df = 2, sig.level = .05, N = 90)
x2 <- pwr.chisq.test(w = w_real, df = 2, sig.level = .05, N = N)



N <- seq(30, 200, 10)

z <- data.frame()

for(i in 1:length(N)){
  x <- pwr.chisq.test(w = w, df = 2, sig.level = .05, N = N[i])
  z1 <- data.frame(w, x$power, N = x$N)
  z <- rbind(z, z1)
}

z$w <- as.factor(z$w)

ggplot(z, aes(x = N, y = x.power, color = w)) +
  theme_bw() +
  geom_line(size = 2) + 
  geom_hline(yintercept = .9, color = "red") +
  labs(y = 'Power', title = "3x2 Chi-Square Power") +
  theme(axis.title = element_text(size = 16)) 



N <- seq(30, 200, 10)
z <- data.frame()

for(i in 1:length(N)){
  x <- pwr.chisq.test(w = w, df = 1, sig.level = .05, N = N[i])
  z1 <- data.frame(w, x$power, N = x$N)
  z <- rbind(z, z1)
}

z$w <- as.factor(z$w)
ggplot(z, aes(x = N, y = x.power, color = w)) +
  theme_bw() +
  geom_line(size = 2) + 
  geom_hline(yintercept = .9, color = "red") +
  labs(y = 'Power', title = "2x2 Chi-Square Power") +
  theme(axis.title = element_text(size = 16)) 

