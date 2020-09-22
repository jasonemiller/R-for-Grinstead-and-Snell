#HorseRace returns the horse, A or B or C or D, that wins a race
horserace<-function(){
  chance<-c(30,40,20,10)
  cumchance<-cumsum(chance)
  x<-sample(1:100,1)
  result<-if (x<cumchance[1]) "A" else
    if (x<cumchance[2]) "B" else
      if (x<cumchance[3]) "C" else "D"
  result
}

#HorseRaces runs n races and reports the proportion of times each horse wins
horseraces<-function(n){
  races<-replicate(n,horserace())
  a<-sum(races=="A")/n
  b<-sum(races=="B")/n
  c<-sum(races=="C")/n
  d<-sum(races=="D")/n
  c(a,b,c,d)
}