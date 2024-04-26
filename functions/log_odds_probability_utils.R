log.odds <- function(p){
  log(p / (1-p))
}
odds <- function(p){
  p / (1- p)
}
p <- function(lo){
  exp(lo) / (1 + exp(lo))
}
