# integration experiment (thin plate version)
library(mgcv)
library(ks)

source("squash.R")
n<-100
#x<-seq(0,1,len=30)
        x <- seq(0, 1,len=n)
        y <- 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * 
            x)^3 * (1 - x)^10


dat<-data.frame(x=x,y=y)

# x values for prediction 
newdat<-data.frame(x=seq(0,0.7,len=n)) 


b<-gam(y~s(x,k=10,bs="ps"),data=dat)

par(mfrow=c(2,2))
plot(x=newdat$x,y=predict(b,newdat),main="no squash",type="l",asp=1,xlim=c(0,1))


# now do some squashing...
lims<-c(0,0.5,0.7,1)
sq<-c(1,1/0.1,1/2)

#lims<-c(0,0.5,1)
#sq<-c(1,1/2)

# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y)


b.s<-gam(y~s(x,k=10,bs="ps"),data=dat.m)

plot(x=newdat$x,y=predict(b.s,newdat),main="squash fit with data",type="l",xlim=c(0,1))
points(x.m,y,main="raw squash data",pch=19,cex=0.3)

##### fixing...
source("psmooth.c.R")

b.fix<-gam(y~s(x,k=10,bs="mdsps"),data=dat.m)


plot(x=newdat$x,y=predict(b.fix,newdat),main="fixed fit (black), truth (red), \ndata, broken fit (green)",type="l",xlim=c(0,1))
lines(x=newdat$x,y=predict(b,newdat),lwd=2,col="red")
lines(x=newdat$x,y=predict(b.s,newdat),col="green",lwd=2)
lines(x=newdat$x,y=predict(b.fix,newdat),lwd=1,col="black")

points(x.m,y,pch=19,cex=0.3)

