# darts program
# simulates throwing n darts at the unit circle centered at (1,1) and reporting the proportion of darts
# that fall in concentric rings of thickness 0.1
darts<-function(n){
  x<-replicate(n,c(runif(1,-1,1),runif(1,-1,1)))
  dist<-sqrt(colSums(x^2))
  miss<-sum(dist>1)
  hits<-dist[dist<=1]
  hits
}

hist(darts(100000),breaks=10,xlim=c(0,1),freq=FALSE)

# -------------------------------------------

# this simulation uses polar coordinates to simulalte the target.
# how does the density plot compare?
polardarts<-function(n){
  x<-replicate(n,c(runif(1,0,1),runif(1,0,2*pi)))
  y<-rbind(x[1,]*cos(x[2,]),x[1,]*sin(x[2,]))
  dist<-sqrt(colSums(y^2))
  miss<-sum(dist>1)
  hits<-dist[dist<=1]
  hits
}

hist(polardarts(100000),breaks=10,xlim=c(0,1),freq=FALSE)
