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

# plot -------------------------------------------------------------------------

library(ggplot2)

# alternative plot #1:
# runs a simulation and plots the progress of wins and losses in a line graph
x<-HTsimulation(40); qplot(c(1:length(unlist(x[3]))),unlist(x[3]),geom="path")

# alternative plot #2:
# of the same thing
plot(c(1:length(unlist(x[3]))),unlist(x[3]), type = "l", lty = 1)



# alternative plot #3:
# same thing AND plot of opponents sumulative wins and losses
y<-as.data.frame(x)
names(y)<-c("aheady","aheadz","cumy","cumz","y","z")
ggplot(y, aes(c(1:40),cumy))+ geom_line()+geom_point()

# plot like Figure 1.2: distribution of end scores
x<-replicate(50,HTsimulation(100))
hist(x)

# plot like Figure 1.3: distribution of numebr times int he lead
x<-replicate(50,HTsimulation_leads(100))
hist(x)
