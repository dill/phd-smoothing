# test the derivatives...

library(mgcv)
set.seed(0) ## simulate some data... 

x<-runif(100)
y<-x^2

b<-gam(y~s(x,k=10))

par(mfrow=c(1,2))

plot(b)

source("smooth.c.R")


b<-gam(y~s(x,k=10,bs="mdstp",xt=list(sq=1,lims=c(0,1))))
plot(b)



