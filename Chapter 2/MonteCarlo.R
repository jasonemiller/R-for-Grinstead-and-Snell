# monte carlo script for grimstead and snell (Chapter 2)

mc1<-function(n){
  x<-runif(n,0,1)
}


# choose a single points in the [a,b]x[c,d] rectangle
mc2<-function(a,b,c,d){
  x<-c(runif(1,a,b),runif(1,c,d))
  x
}

# monte carlo simulation for estimating the area under y=x^2
y<-replicate(10000,mc2(0,1,0,1))
sum(y[1,]^2<y[2,])/length(y)
sum((y[1,]-1/2)^2+(y[2,]-1/2)^2<=1/4)/length(y[1,])

# monte carlo simulation for estimating the area under y=x^2
y<-replicate(10000,mc2(0,pi,0,1))
sum(cos(y[1,]^2)<y[2,])/length(y)
