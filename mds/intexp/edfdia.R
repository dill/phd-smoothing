# see what models are fit when they have the same EDF

set.seed(1)

# import some libraries and do the setup as in tpintexp.R


library(mgcv)
# code to do the squashing
source("squash.R")
# code for the thin plate adjustment
source("smooth.c.R")

n<-100
x<-seq(0,1,len=n)
y <- 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10*x)^3 * (1 - x)^10
# x values for prediction 
newdat<-data.frame(x=x)

k<-80
method<-"GCV.Cp"
#method<-"REML"
basis<-"mdstp"

noise<-rep(0,length(x))
noise<-rnorm(length(x))*0.1

dat<-data.frame(x=x,y=y+noise)

###### now do some squashing...
lims<-c(0,0.4,0.6,0.8,1)
sq<-c(1/0.09,1,1/20,1)
# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y+noise)
newdat.m<-data.frame(x=x.m)


##### fixing...
alim<-0
blim<-1
N<-1000
xs<-alim+(1:N -0.5)*(blim-alim)/N
dens<-c(rep(sq[1],sum(xs<=lims[2])),
        rep(sq[2],sum(xs>lims[2] & xs<=lims[3])),
        rep(sq[3],sum(xs>lims[3] & xs<=lims[4])),
        rep(sq[4],sum(xs>lims[4] & xs<=lims[5])))


# now time to fit some models

par(mfrow=c(3,2))

# EDF=71 (ish)
# set sp
sp.fix<-4.000971e-05 
sp.s<-0.000000001

b.fix<-gam(y~s(x,k=k,bs=basis,xt=list(dens=dens)),data=dat.m,method=method,sp=sp.fix)

b.s<-gam(y~s(x,k=k),data=dat.m,method=method,sp=sp.s)

cat("unfixed EDF=",sum(b.s$edf),"\n")
cat("fixed EDF=",sum(b.fix$edf),"\n")
cat("correlation=",cor(predict(b.s,newdat.m),predict(b.fix,newdat.m)),"\n")


# plot in squashed space
plot(x=newdat.m$x,y=predict(b.fix,newdat.m),main="adjusted fit (green), data (black),\n unadjusted fit (blue)",type="n",xlim=c(0,max(x.m)),xlab="x",ylab="y")
lines(x=newdat.m$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat.m$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
points(x.m,y,pch=19,cex=0.3)
# unsquashed space
plot(x=x,y=y,main="adjusted fit (green), truth (red), \nunadjusted fit (blue)",type="n",xlim=c(0,1),xlab="x",ylab="y")
lines(x=x,y=y,lwd=2,col="red")
lines(x=newdat$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
points(x=x,y=y+noise,pch=19,cex=0.3)


## EDF= 19
# set sp
sp.fix<-10000 
sp.s<-0.0002

b.fix<-gam(y~s(x,k=k,bs=basis,xt=list(dens=dens)),data=dat.m,method=method,sp=sp.fix)

b.s<-gam(y~s(x,k=k),data=dat.m,method=method,sp=sp.s)

cat("unfixed EDF=",sum(b.s$edf),"\n")
cat("fixed EDF=",sum(b.fix$edf),"\n")
cat("correlation=",cor(predict(b.s,newdat.m),predict(b.fix,newdat.m)),"\n")


# plot in unsquashed space
plot(x=x,y=y+noise,main="adjusted fit (green), data (black),\n unadjusted fit (blue)",type="n",xlim=c(0,max(x.m)),xlab="x",ylab="y")
lines(x=newdat.m$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat.m$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
points(x.m,y,pch=19,cex=0.3)
# unsquashed space
plot(x=x,y=y,main="adjusted fit (green), truth (red), \nunadjusted fit (blue)",type="n",xlim=c(0,1),xlab="x",ylab="y")
lines(x=x,y=y,lwd=2,col="red")
lines(x=newdat$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
points(x=x,y=y+noise,pch=19,cex=0.3)

## EDF=42
# set sp
sp.fix<-0.05 
sp.s<-0.00000025

b.fix<-gam(y~s(x,k=k,bs=basis,xt=list(dens=dens)),data=dat.m,method=method,sp=sp.fix)

b.s<-gam(y~s(x,k=k),data=dat.m,method=method,sp=sp.s)

cat("unfixed EDF=",sum(b.s$edf),"\n")
cat("fixed EDF=",sum(b.fix$edf),"\n")
cat("correlation=",cor(predict(b.s,newdat.m),predict(b.fix,newdat.m)),"\n")


# plot in unsquashed space
plot(x=x,y=y+noise,main="adjusted fit (green), data (black),\n unadjusted fit (blue)",type="n",xlim=c(0,max(x.m)),xlab="x",ylab="y")
lines(x=newdat.m$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat.m$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
points(x.m,y,pch=19,cex=0.3)
# unsquashed space
plot(x=x,y=y,main="adjusted fit (green), truth (red), \nunadjusted fit (blue)",type="n",xlim=c(0,1),xlab="x",ylab="y")
lines(x=x,y=y,lwd=2,col="red")
lines(x=newdat$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
lines(x=newdat$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
points(x=x,y=y+noise,pch=19,cex=0.3)
