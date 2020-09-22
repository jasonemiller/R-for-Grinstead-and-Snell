
# function: tosscoin() gives the result of tossing a fair coin
tosscoin <- function(){
  x<-runif(1,0,1)
  if (x<1/2) "H" else "T"
}

# function:  heads(x) where x is a vector of characters 'H' and 'T' returns
# the number of times 'H' appears
heads<-function(x){
  sum(x=="H")
}

# function:  tosscoins(n) where n is a counting number tosses a fair coin n
# times and returns the number of times it comes up heads
tosscoins <- function(n){
  heads(replicate(n,tosscoin()))
}

# EXERCISE 1 # -----------------------------------------------------

ex1<-function(n){
  x<-replicate(n,tosscoin())
  k<-floor(n/100) #number of sequences of length 100
  y<-rep(0,k+1)
  for(i in 1:k)
    y[i]<-heads(x[c(1:(i*100))])/(i*100)-1/2
  y[k+1]<-heads(x)/length(x)-1/2
  y
}

ex11<-function(n){
  x<-replicate(n,tosscoin())
  k<-floor(n/100) #number of sequences of length 100
  y<-rep(0,k+1)
  z<-y
  for(i in 1:k)
    y[i]<-heads(x[c(1:(i*100))])/(i*100)-1/2
  y[k+1]<-heads(x)/length(x)-1/2
  for(i in 1:k)
    z[i]<-heads(x[c(1:(i*100))])-i*50
  z[k+1]<-heads(x)-1/2*length(x)
  list(y,z)
}

# EXERCISE 2 # -----------------------------------------------------

ex2<-function(n){
  x<-replicate(n,tosscoin())
  p<-heads(x)/length(x)-1/2
  if(abs(p)<.1) "Y" else "N"
}

repex2<-function(n){
  sum(replicate(100,ex2(n))=="Y")
}

# EXERCISE 3 # -----------------------------------------------------

# sum the roll of three dice
galileo<-function(n){
  x<-replicate(n,sum(sample(1:6,3,replace=TRUE)))
  a<-sum(x==9)/n
  b<-sum(x==10)/n
  list(a,b)
}

# EXERCISE 4 # -----------------------------------------------------

matches=1000
score_A=0
score_B=0
serve_A=1 # A serves first
serve_B=0 
win_A=0

# Player_A serves with 60% probability of winning the volley and 50% if he
# does not serve
for(i in 1:matches){
  while(score_A<21 && score_B<21){
    # A serves first
    if(serve_A==1){
      if(runif(1,0,1)<0.6){
        score_A<-score_A+1
        else
          serve_B=1
        serve_A=0 #Change Serve
      }
    }
    if(serve_B==1){
      if(runif(1,0,1)<0.5){
        score_B<-score_B+1
        else
          serve_B=0
        serve_A=1
      }
    }
  }
  if(score_A>score_B){
    win_A<-win_A+1
  }
  score_A=0
  score_B=0
  serve_A=1
  serve_B=0 # A serves first
}

# sprintf('A won %d percent of %d mactches',win_A/matches*100,matches)


