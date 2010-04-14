# integration experiment (thin plate version)
library(mgcv)
library(ks)

# code to do the squashing
source("squash.R")
# code for the thin plate adjustment
source("smooth.c.R")
# code for the p-spline adjustment
source("psmooth.c.R")


n<-300
x<-seq(0,1,len=n)
#y<-x^2
y <- 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10*x)^3 * (1 - x)^10
# x values for prediction 
newdat<-data.frame(x=seq(0,1,len=n)) 

k<-100
method<-"GCV.Cp"
#method<-"REML"
basis<-"mdsps"
basis<-"mdstp"

dat<-data.frame(x=x,y=y)

b<-gam(y~s(x,k=k),data=dat)

par(mfrow=c(2,2))
plot(x=newdat$x,y=predict(b,newdat),main="no squash",type="l",xlim=c(0,1),col="red")


###### now do some squashing...
lims<-c(0,0.4,0.6,0.8,1)
sq<-c(1/0.05,1,1/20,1)

#lims<-c(0,0.5,1)
#sq<-c(1,1/2)

# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y)

newdat.m<-data.frame(x=squash(newdat$x,lims,sq))

b.s<-gam(y~s(x,k=k),data=dat.m)

plot(x=newdat.m$x,y=predict(b.s,newdat.m),main="squash fit with data",type="l",xlim=c(0,max(x.m)),col="blue")
points(x.m,y,main="raw squash data",pch=19,cex=0.3)



##### fixing...

b.fix<-gam(y~s(x,k=k,bs=basis),data=dat.m)

plot(x=newdat.m$x,y=predict(b.fix,newdat.m),main="fixed fit (green), truth (red), \ndata, broken fit (blue)",type="n",xlim=c(0,max(x.m)))
lines(x=newdat.m$x,y=predict(b,newdat.m),lwd=2,col="red")
lines(x=newdat.m$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat.m$x,y=predict(b.fix,newdat.m),col="green",lwd=2)

points(x.m,y,pch=19,cex=0.3)

# now unsquash the predicted values...

plot(x=newdat$x,y=predict(b,newdat),main="fixed fit (green), truth (red), \ndata, broken fit (blue)",type="n",xlim=c(0,1))
lines(x=newdat$x,y=predict(b,newdat),lwd=2,col="red")
lines(x=newdat$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat$x,y=predict(b.fix,newdat.m),col="green",lwd=2)





