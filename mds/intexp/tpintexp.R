# integration experiment (thin plate version)
library(mgcv)
library(ks)

source("squash.R")

x<-seq(0,1,len=30)
y<-x^2

dat<-data.frame(x=x,y=y)

# x values for prediction 
newdat<-data.frame(x=seq(0,1,len=120)) 


b<-gam(y~s(x,k=10),data=dat)

par(mfrow=c(2,2))
plot(x=newdat$x,y=predict(b,newdat),main="no squash",type="l",asp=1,xlim=c(0,1))


# now crazy things happen
# move around the values of x and xk
xk<-seq(1/8,7/8,len=8) #choose some knots 
xp<-seq(0,1,len=100) # xvaluesforprediction 

lims<-c(0,0.5,0.7,1)
sq<-c(1,1/0.1,1/2)

lims<-c(0,0.5,1)
sq<-c(1,1/2)

# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y)


b.s<-gam(y~s(x,k=10),data=dat.m)

plot(x=newdat$x,y=predict(b.s,newdat),main="squash fit with data",type="l",asp=1,xlim=c(0,1))
points(x.m,y,main="raw squash data",pch=19,cex=0.3,asp=1,xlim=c(0,1))

##### fixing...
source("smooth.c.R")

b.fix<-gam(y~s(x,k=10,bs="mdstp"),data=dat.m)


plot(x=newdat$x,y=predict(b.fix,newdat),main="fixed fit (black), truth (red), data, broken fit (green)",type="l",asp=1,xlim=c(0,1))
lines(x=newdat$x,y=predict(b,newdat),lwd=2,col="red")
lines(x=newdat$x,y=predict(b.s,newdat),col="green",lwd=2)
lines(x=newdat$x,y=predict(b.fix,newdat),lwd=1,col="black")

points(x.m,y)

