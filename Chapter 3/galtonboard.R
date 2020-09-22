# galton board (p.99)

# simulate the result of a bal hitting a pin
# -1 = fall left, 1 = fall right
fall<-function(p){
  sample(c(-1,1),1,prob=c(1-p,p))
}

# simulate the falling of a single ball through an n-level board
galton<-function(n,p){
  sum(replicate(n,fall(p)))
}

# simulate the foll of N balls through an n-level board
galtonboard<-function(N,n,p){
  replicate(N,galton(n,p))
}

# ---------------------------

# visualize the result of the simulation with a histogram
N<-10000
n<-40
x<-galtonboard(N,n,.5)
hist(x,xlab="Bin location",ylab="Number of balls in the bin",main=sprintf("Result of a Galton Board experiment \n with %d balls and %d levels", N,n))