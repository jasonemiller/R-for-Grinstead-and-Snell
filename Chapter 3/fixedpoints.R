# generate a random permutation on n elements
# note: the notation means that the i is permuted to x[i]
randompermutation<-function(n){
  x<-sample(n,n,replace=FALSE)
  x
}

# function generates a random permutation on a set of size n and counts
# the number of fixed points it has
fixedpoints<-function(n){
  identity<-c(1:n)
  x<-randompermutation(n)
  y<-rbind(identity,x)
  sum(identity==x)
}

# ===================================

randompermutation(3)

fixedpoints(5)
