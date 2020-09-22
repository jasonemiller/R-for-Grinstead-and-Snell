# birthday function (p.78):  the probability that n people
# do not share a birthday
birthday<-function(n){
  x<-choose(365,n)*factorial(n)/365^n
  x
}

# a list of the probability for the first 30 choices
birthday(20:25)
