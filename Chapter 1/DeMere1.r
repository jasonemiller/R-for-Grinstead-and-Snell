dieroll <- function(n){
  ceiling(runif(1,0,n))
}
# alternative:  sample(1,1:6,replacement=TRUE)

kdieroll<-function(k,n){
  replicate(k,dieroll(n))
}

# counts the number of 6s that appear when four 6-sided dice are rolled
demere<-function(){
  sum(kdieroll(4,6)=="6")
}

# repeats the demere experiment n times and returns the proportion of times
# that a six appears in an experiment as well as the list of experiment results
# USE:  use unlist() to strip the list structure from components
demere1 <- function(n){
  x<-replicate(n,demere())
  N<-sum(x>0)/n
  list(N,x)
}
