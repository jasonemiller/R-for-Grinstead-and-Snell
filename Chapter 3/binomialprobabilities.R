# binomial probabilities, p.98
# calculate the kth term of the nth binomial expansion for p
bprob<-function(n,p,k){
#  print(paste("b(p,n,k) = ", choose(n,k)*p^k*(1-p)^(n-k)))
  choose(n,k)*p^k*(1-p)^(n-k)
}

# given a range of binomial terms for a fixed p and n, prints them out
showbprob<-function(n,p,kmin,kmax){
  for (i in kmin:kmax)
    print(paste("b(",p,",",n,",",i,") =",choose(n,i)*p^i*(1-p)^(n-i)),sep="")  
}

# sums a range of binomial probabilities for a fixed p and n, prints them out
sumbprob<-function(n,p,kmin,kmax){
  i<-kmin:kmax
  sum(choose(n,i)*p^i*(1-p)^(n-i)) 
}

# -------------------------------------------

# n=100,p=1/2, kmin=45 and kmax=55
showbprob(100,.5,45,55)
sumbprob(100,.5,45,55)
