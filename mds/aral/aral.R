# Aral Sea data set analysis

# run from phd-smoothing/mds


# libraries
library(mgcv)
library(soap)

# mds files
source("mds.R")

# load the data and boundary
aral<-read.csv("aral/aral.dat",sep=" ")
bnd<-read.csv("aral/aralbnd.csv")
bnd<-bnd[,c(2,3)]


# first cut out the crap using inSide
onoff<-inSide(bnd,aral$lo,aral$la)

aral.dat<-data.frame(la=aral$la[onoff],
                     lo=aral$lo[onoff],
                     chl=aral$chl[onoff])

# plot setup
par(mfrow=c(2,2))

# plot some raw data
aral$chl[!onoff]<-NA
image(x=unique(aral$la),y=unique(aral$lo),z=matrix(aral$chl,46,46),
      asp=1,main="raw data with boundary")
lines(bnd)

# fit a thin plate model
tp.fit<-gam(chl~s(lo,la,k=49),data=aral.dat)

# prediction grid
m<-50;n<-50
xm <- seq(min(aral.dat$lo),max(aral.dat$lo),length=m)
yn<-seq(min(aral.dat$la),max(aral.dat$la),length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
pred.onoff<-inSide(bnd,xx,yy)
pred.grid<-data.frame(lo=xx[pred.onoff],la=yy[pred.onoff])

tp.pred<-predict(tp.fit,newdata=pred.grid)
pred.mat<-matrix(NA,m,n)
pred.mat[pred.onoff]<-tp.pred
image(pred.mat,x=unique(xx),y=unique(yy),main="tprs")

