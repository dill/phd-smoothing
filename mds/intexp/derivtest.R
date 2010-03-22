# test the derivatives...

library(mgcv)
set.seed(0) ## simulate some data... 

x<-runif(100)
y<-x^2

b<-gam(y~s(x,k=10))




