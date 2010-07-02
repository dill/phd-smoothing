# integration experiment (thin plate version)
library(mgcv)
#library(ks)

# code to do the squashing
source("squash.R")
# code for the thin plate adjustment
source("smooth.c.R")
# code for the p-spline adjustment
#source("psmooth.c.R")

n<-100 
#n<-200 # changing to 200 shows the wiggles
x<-seq(0,1,len=n)
#y<-x^2
y <- 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10*x)^3 * (1 - x)^10
# x values for prediction 
newdat<-data.frame(x=x) 

k<-60
method<-"GCV.Cp"
#method<-"REML"
basis<-"mdsps"
basis<-"mdstp"

noise<-rep(0,length(x))
#noise<-rnorm(length(x))*0.1

dat<-data.frame(x=x,y=y+noise)

b<-gam(y~s(x,k=k),data=dat,method=method)

#par(mfrow=c(2,2))
#plot(x=newdat$x,y=predict(b,newdat),main="unsquahed function",type="l",xlim=c(0,1),col="red",xlab="x",ylab="y")


###### now do some squashing...
lims<-c(0,0.4,0.6,0.8,1)
sq<-c(1/0.09,1,1/20,1)

#lims<-c(0,0.5,1)
#sq<-c(1,1/2)

# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y+noise)

newdat.m<-data.frame(x=x.m)

b.s<-gam(y~s(x,k=k),data=dat.m,method=method)

#plot(x=x.m,y=y,main="squashed function",type="l",xlim=c(0,max(x.m)),col="black",xlab="x",ylab="y")


##### fixing...
alim<-min(x)
blim<-max(x)
#alim<-0
#blim<-1
N<-1000
xs<-alim+(1:N -0.5)*(blim-alim)/N


dens<-c(rep(sq[1],sum(xs<=lims[2])),
        rep(sq[2],sum(xs>lims[2] & xs<=lims[3])),
        rep(sq[3],sum(xs>lims[3] & xs<=lims[4])),
        rep(sq[4],sum(xs>lims[4] & xs<=lims[5])))

b.fix<-gam(y~s(x,k=k,bs=basis,xt=list(dens=dens)),data=dat.m,method=method)


par(mfrow=c(2,1))

plot(x=newdat.m$x,y=predict(b.fix,newdat.m),main="adjusted fit (green), data (black),\n unadjusted fit (blue)",type="n",xlim=c(0,max(x.m)),xlab="x",ylab="y")
points(x.m,y,pch=19,cex=0.3)
lines(x=newdat.m$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat.m$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
rug(newdat.m$x)


# now unsquash the predicted values...

plot(x=newdat$x,y=predict(b,newdat),main="adjusted fit (green), truth (red), \nunadjusted fit (blue)",type="n",xlim=c(0,1),xlab="x",ylab="y")
points(x=x,y=y+noise,pch=19,cex=0.3)
lines(x=newdat$x,y=predict(b,newdat),lwd=2,col="red")
lines(x=newdat$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
rug(newdat$x)





