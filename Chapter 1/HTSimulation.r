#HTSimulation
# NOTE: use unlist() to strip structure from reported elements
HTsimulation<-function(n){
  # the experiment: record of wining for Player1
  y<-sample(c(-1,1),size=n,replace=TRUE)
  # record of wining for Player2
  z<- -y
  # running winnings of Player1, and # times Player1 is ahead
  cy<-cumsum(y)
  aheady<-sum(cumsum(y)>0)
  # running winnings of Player2, and # times Player2 is ahead
  cz<-cumsum(z)
  aheadz<-sum(cumsum(z)>0)
  # reporting all calculations
  list(aheady,aheadz,cy,cz,y,z)
  }

# return the number of wins in a simulation of n tosses
HTsimulation_wins<-function(n){
  # the experiment: record of wining for Player1
  y<-sample(c(-1,1),size=n,replace=TRUE)
  cy<-cumsum(y)
  # reporting all calculations
  cy[n]
}

#HTSimulation
# NOTE: use unlist() to strip structure from reported elements
HTsimulation_leads<-function(n){
  # the experiment: record of wining for Player1
  y<-sample(c(-1,1),size=n,replace=TRUE)
  # running winnings of Player1, and # times Player1 is ahead
  cy<-cumsum(y)
  aheady<-sum(cumsum(y)>0)
}