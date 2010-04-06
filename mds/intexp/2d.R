# two dimensional smoothing adjustments...

par(mfrow=c(2,2))

# load MASS for bivar Normal
library(MASS)

library(mgcv)
library(soap)

source("squash.R")

set.seed(1)

# make a sample
samp<-mvrnorm(1000,c(0,0),Sigma=matrix(c(1,0,0,1),2,2))

# do a kde fit to get the grid
res<-20 # grid resolution
lims<-c(-1,1,-1,1) # limits
bivn.kde <- kde2d(samp[,1], samp[,2], n =res, lims=lims)

# calculate the grid
x<-seq(lims[1],lims[2],len=res)
y<-seq(lims[3],lims[4],len=res)
xx<-rep(x,res)
yy<-rep(y,rep(res,res))

dat<-data.frame(x=xx,y=yy,z=as.vector(bivn.kde$z))

# raw data plot

image(matrix(dat$z,res,res),x=bivn.kde$x,y=bivn.kde$y,main="raw data",asp=1)

b<-gam(z~s(x,y,k=10),data=dat)

vis.gam(b,plot.type="contour",main="tprs fit",asp=1)


###################################
# now something interesting

# explode top right corner
xsq<-squash(xx,c(-1,0,1),c(1,2))
ysq<-squash(yy,c(-1,0,1),c(1,2))

dat.sq<-data.frame(x=xsq,y=ysq,z=as.vector(bivn.kde$z))


b<-gam(z~s(x,y,k=10),data=dat.sq)

vis.gam(b,plot.type="contour",main="squash tprs fit",asp=1)
