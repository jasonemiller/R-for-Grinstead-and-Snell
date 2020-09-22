# generate plots of the binomial distribution
# the number of 'successes' in a n Bernoulli trials
bernevent<-function(p){
  bt<-sample(c(0:1),1,prob=c(1-p,p))
  bt
}

# simulate a bernoulli trial of n events, where a success has
# probability p; return the number of successes
berntrial<-function(n,p){
  sum(replicate(n,bernevent(p)))
}

binomialplot<-function(N,n,p){
  x<-replicate(N,berntrial(n,p))
  hist(x)
}

# --------------------------------------------

# number of successes in bernoulli trials of n=10 events where
# success has probability p=0.5 and the plot of the distribution
binomialplot(1000,10,.5)

# number of successes in bernoulli trials of n=100 events where
# success has probability p=0.5 and the plot of the distribution
binomialplot(1000,100,.5)


# --------------------------------------------

# putting two or more histograms on the same axes 
# from https://www.dataanalytics.org.uk/plot-two-overlapping-histograms-on-one-chart-in-r/

# FIRST - create the histograms, naming each to a different variable
h1<-binomialplot(1000,10,.5)
h2<-binomialplot(1000,100,.5)

# SECOND - collect information from each to know the range of the inputs (bins)
# and the range of out outputs (counts)
xrange<-range(c(h1$breaks,h2$breaks))
yrange<-max(c(h1$count,h2$count))

# THIRD - plot, using options that take into account the horizontal and
# vertical ranges; also note
#   * set colors to be different using the "col=" option; the fourth RGB
#     value sets opacity to allow for some tranparency
#   * in additional plots, the "add=TRUE" option tells R to add the new
#     plot to the old plot
plot(h1,xlim=c(min(xrange),max(xrange)),ylim=c(0,yrange),col=rgb(0,0,1,0.5))
plot(h2,add=TRUE,col=rgb(1,0,0,0.5))
