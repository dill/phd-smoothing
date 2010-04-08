# test the squash function
source("squash.R")

n<-100
x <- seq(0, 1,len=n)
y <- 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10*x)^3 * (1 - x)^10

par(mfrow=c(2,2))

plot(x,y,type="p",main="no squash")
abline(v=c(0,0.5,0.7,1),col="green",lwd=2)

# now do some squashing...

lims<-c(0,0.5,1)
sq<-c(0.1,1/3)

# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y)

plot(x.m,y,type="p",main="some squash")
abline(v=lims,col="green",lwd=2)
abline(v=lims[2:length(lims)]/sq,lwd=2,col="blue")

# different squashing
lims<-c(0,0.5,0.7,1)
sq<-c(1/2,8,1/4)
# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y)

plot(x.m,y,type="p",main="different squash")
abline(v=c(0,0.5,0.7,1),col="green",lwd=2)
abline(v=lims[2:length(lims)]/sq,lwd=2,col="blue")
