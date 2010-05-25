# 2d testing of the integration

source("smooth2NA.c.R")

# from the example in gam()
library(mgcv)
library(soap)
set.seed(0) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)

dat<-data.frame(x=dat$x0,y=dat$x1,z=dat$y)


b1<-gam(z~s(x,y),data=dat)
par(mfrow=c(2,2))
vis.gam(b1,plot.type="contour")

bnd.mds<-list(x=c(1,1,0,0,1),y=c(1,0,0,1,1))


b2<-gam(z~s(x,y,bs="mdstp",xt=list(bnd.mds=bnd.mds,
                                     bnd=bnd.mds,
                                     op=NULL,
                                     mds.obj=NULL
         )),data=dat)

vis.gam(b2,plot.type="contour")

