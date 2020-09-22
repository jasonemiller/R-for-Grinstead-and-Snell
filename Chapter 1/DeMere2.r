# from http://ditraglia.com/Econ103Public/Rtutorials/Rtutorial4.html
# experimentA is the same as demere, but done differently
experimentA <- function(){
  rolls <- sample(1:6, size = 4, replace = TRUE)
  condition <- sum(rolls == 6) > 0
  return(condition)
}
# experimentB is a single run of Demere2 experiment
experimentB <- function(){
  first.die <- sample(1:6, size = 24, replace = TRUE)
  second.die <- sample(1:6, size = 24, replace = TRUE)
  condition <- sum((first.die == second.die) & (first.die == 6)) > 0
  return(condition)
}

# NOTE: summing the simulation
simsA <- replicate(100000, experimentA())
sum(simsA)/length(simsA)
#
simsB <- replicate(1000000, experimentB())
sum(simsB)/length(simsB)
