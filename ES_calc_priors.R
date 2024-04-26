es <- function(mean = 0, sd = 1){
  conversion_factor <- sqrt(3)/pi 
  small <- .2 / conversion_factor 
  medium <- .5 / conversion_factor
  large <- 1.45 / conversion_factor
  no_effect <- round(diff(pnorm(c(-small, small), mean, sd)), 2)
  small_e <- round(diff(pnorm(c(-medium, medium), mean, sd)) - diff(pnorm(c(-small, small), mean, sd)), 2) 
  medium_e <- round(diff(pnorm(c(-large, large), mean, sd)) - diff(pnorm(c(-medium, medium), mean, sd)), 2) 
  large_e <- 1- round(diff(pnorm(c(-large, large), mean, sd)), 2)  
  print(paste0("No Effect: ", no_effect, " | Small Effect: ", small_e, " | Medium Effect: ", medium_e, " | Large Effect: ", large_e))
} 

es(mean = 0, sd = 1)
es(mean = 0, sd = 2)

es(mean = 1, sd = 2)
es(mean = 1, sd = 1)

pp <- rnorm(1e5, 0, 2)
plot(density(pp))
abline(v = c(-.37, .37), lty = 2, col= "green")
abline(v = c(-.9, .9), lty = 4, col = "blue")
abline(v = c(-2.63, 2.63), lty = 6, col = "red")
