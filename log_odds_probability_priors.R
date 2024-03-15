rm(list = ls())
library(gridExtra)
library(gridGraphics)
library(RColorBrewer)

set.seed(42)

log.odds <- function(p){
  log(p / (1-p))
}
odds <- function(p){
  p / (1- p)
}
p <- function(lo){
  exp(lo) / (1 + exp(lo))
}

n <- 1e3
scale <- 3 
my_colors <- RColorBrewer::brewer.pal(6, "Set3")[1:6]


dist_c <- rnorm(n, mean = 0, sd = 2)
dist_neg1 <- rnorm(n, mean = -.5, sd = 2)
dist_neg2 <- rnorm(n, mean = -1, sd = 2)
dist_neg3 <- rnorm(n, mean = -1.5, sd = 2)
dist_pos1 <- rnorm(n, mean = .5, sd = 2)
dist_pos2 <- rnorm(n, mean = 1, sd = 2)


png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\log_odds_priors.png", width = 480*scale, height = 480*scale, res = 72*scale)
plot(NULL, xlim = c(-7,7), ylim = c(0, .3), xlab = "Log-Odds", ylab = "Density", main = "Priors: Log-Odds Scale")
lines(density(dist_neg3), col = my_colors[1], lwd = 5)
lines(density(dist_neg2), col = my_colors[2], lwd = 5)
lines(density(dist_neg1), col = my_colors[3], lwd = 5)
lines(density(dist_c), col = my_colors[4], lwd = 5)
lines(density(dist_pos1), col = my_colors[5], lwd = 5)
lines(density(dist_pos2), col = my_colors[6], lwd = 5)
dev.off()

priors <- seq(-1.5, 1, .5)
p(priors)

intercepts <- c(-1.93, -1.3, -.73, .3, 1.67)
p(intercepts)
ps <- diff(c(0, p(intercepts)))


png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\probability_priors.png", width = 480*scale, height = 480*scale, res = 72*scale)
plot(NULL, xlim = c(0,1), ylim = c(0, 3), xlab = "Probability", ylab = "Density", main = "Priors: Probability Scale")
lines(density(p(dist_neg3)), col = my_colors[1], lwd = 5)
lines(density(p(dist_neg2)), col = my_colors[2], lwd = 5)
lines(density(p(dist_neg1)), col = my_colors[3], lwd = 5)
lines(density(p(dist_c)), col = my_colors[4], lwd = 5)
lines(density(p(dist_pos1)), col = my_colors[5], lwd = 5)
lines(density(p(dist_pos2)), col = my_colors[6], lwd = 5)
dev.off()



lo <- seq(-4, 4, length.out = 1000)
probs <- p(lo)
o <- odds(probs)

png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\probability_logodds.png", width = 480*scale, height = 480*scale, res = 72*scale)
plot(probs, lo, type = "l", lwd = 4, xlab = "Probability", ylab = "Log-Odds", main = "Log-Odds vs. Probability")
abline(a = 0, b = 0, lty = 2)
abline(v = .5, lty = 2)
dev.off()

s <- 1.68
m <- 3.47
l <- 6.71

s.lo <- lo[n - sum(o >= s)]
m.lo <- lo[n - sum(o >= m)]
l.lo <- lo[n - sum(o >= l)]


plot(lo, o, type = "l", lwd = 7)
abline(h = s, col = my_colors[1], lwd = 3)
abline(h = m, col = my_colors[3], lwd = 3)
abline(h = l, col = my_colors[4], lwd = 3)
abline(v = lo[n - sum(o >= s)], col = my_colors[1], lwd = 3)
abline(v = lo[n - sum(o >= m)], col = my_colors[3], lwd = 3)
abline(v = lo[n - sum(o >= l)], col = my_colors[4], lwd = 3)





plot(probs, o)
abline(v = .5)

set.seed(42)
beta_prior <- rnorm(n, mean = 0, sd = 2)

png("C:\\Users\\Sean\\OneDrive - McMaster University\\LIVELab\\Social_Drumming\\Manuscript\\beta_log_odds_prior.png", width = 480*scale, height = 480*scale, res = 72*scale)
plot(NULL, xlim = c(-6,6), ylim = c(0, .2), xlab = "Log-Odds", ylab = "Density", main = "Priors: Log-Odds Scale")
lines(density(beta_prior), col = my_colors[4], lwd = 5)
abline(v = s.lo, lty = 2)
abline(v = s.lo * -1, lty = 2)
abline(v = m.lo, lty = 4)
abline(v = m.lo * -1, lty = 4)
abline(v = l.lo, lty = 6)
abline(v = l.lo * -1, lty = 6)
text(.5, 0, "Small Effect")
text(1.4, .025, "Medium Effect")
text(1.9, .05, "Large Effect")
dev.off()

diff(pnorm(c(-1.9, 1.9), sd = 1))
