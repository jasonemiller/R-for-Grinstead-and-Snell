# records (p.84)
# for a random permutation of c(1:n), counts the number of times
# x[i]>x[j] for j<i.

randompermutation<-function(n){
  x<-sample(n,n,replace=FALSE)
  x
}

# given a vector, find the maximum of its first i entries
maxyet<-function(x,i){
  y<-max(x[1:i])
  y
}

# calculates a vector or progressive maxiomum values
# the ith value is the maximum of 1:i of the input vector
cummaxyet<-function(x){
  for (i in 1:length(x))
    x[i]<-maxyet(x,i)
  x
}

# calculate the number of 'new record' entries in a vector
records<-function(x){
  y<-cummaxyet(x)
  sum(na.omit(diff(y)/diff(y)))+1
}

# -------------------------------

# simulate finding the number of records in random permutations of
# length 10
x<-replicate(100,records(randompermutation(10)))
hist(x)
mean(x)

# simulate finding the number of records in random permutations of
# length 10
x<-replicate(100,records(randompermutation(20)))
hist(x)
mean(x)

# simulate finding the number of records in random permutations of
# length 10
x<-replicate(100,records(randompermutation(30)))
hist(x)
mean(x)


