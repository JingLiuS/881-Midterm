library(ggplot2)
library(qualityTools)



# create a vector of w exponential waiting times with lambda = lam
set.seed(50)
wait <- function(w,lam){
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))
  }
  return(a)
}

# create a vector of 10000 exponential waiting times with lambda = 5
exp1 <- wait(10000, 5)
head(exp1)

# use histogram to get a general idea about the distribution of exp1
hist(exp1)

# add a exponential curve to fit it
x <- seq(min(exp1), max(exp1), 0.1)
y <- dexp(x, 5)*length(exp1)*0.1
lines(x, y, col = "red")

# calculate the mean and variance to verify exponential distribution
mean(exp1)
var(exp1)

# change the lambda to see how the distribution varies
exp2 <- wait(10000, 1)
hist(exp2)

exp3 <- wait(10000, 10)
hist(exp3)



# create a vector of exponential waiting times which total t <= Max with lambda = lam
set.seed(50)
wait.until <- function(Max,lam){
  time = 0
  a = NULL
  while(time < Max){
    inter = rexp(1,lam)
    a = c(a,inter)
    time = time + inter
  }
  return(a[1:(length(a)-1)])  ##test w seed ## haha use ()
}

# now simulate the number of events to show that the number of events divided by
# exponential waiting times are Poisson distributed
# (don't forget to comment out the "set.seed")

poi.test <- function(rep, Max, lam){
  a = NULL
  for(i in 1:rep){
    q = wait.until(Max,lam)
    a = c(a,length(q))
  }
  return(a)
}

# create a dataset using poi.test()
poi1 <- poi.test(10000, 1, 5)
head(poi1)

# use histogram to get a general idea about the distribution of poi1
hist(poi1)

# add a poison curve
x <- seq(min(poi1), max(poi1), 1)
y <- dpois(x, 5)*length(poi1)
lines(x, y, col = "red")

# verify poison distribution 
mean(poi1)
var(poi1)

# change the parameters
poi2 <- poi.test(10000, 5, 5)
hist(poi2)

poi3 <- poi.test(10000, 1, 10)
hist(poi3)

# we can see that the parameter for this poison distribution 
# is equal to the product of Max and lam
# where the Max measures the cumulative time
# and lam is the parameter for exponential distribution
# that generated the time intervels



# now simlate the waiting time for k events to occur with lambda = lam
set.seed(50)
wait.for <- function(k, lam){
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)
    count = count + 1
    time = time+inter
  }
  
  return(time)
} 

gam.test <-function(rep, k, lam ){
  a=NULL
  for (i in 1:rep){
    t = wait.for(k,lam)
    a = c(a,t)
  }
  return(a)
}
  
# create a dataset using gam.test
gam1 <- gam.test(10000, 6, 5)
head(gam1)  

# draw histogram
hist(gam1, breaks = 30)
  
# add a gamma curve
shape <- mean(gam1)^2/var(gam1)
lambda <- mean(gam1)/var(gam1)
x <- seq(min(gam1), max(gam1), 0.1)
y <- dgamma(x, shape, rate = lambda)*length(gam1)*0.1
lines(x, y, col = "red")

# calculate the parameters
mean(gam1)
var(gam1)
lambda <- mean(gam1)/var(gam1)
lambda
alpha <- mean(gam1)^2/var(gam1)
alpha

# for a gamma distribution with parameters of alpha (i.e. the shape parameter) and lambda
# alpha is equal to the number of events occurred
# and lambda is equal to the parameter of the exponential distribution
# that generates the time intervals between events


