# 2d testing of the integration

library(mgcv)
library(soap)
source("mds.R")
source("inSide.R")
#source("intexp/smooth2NA.c.R")

# from the example in gam()
set.seed(0) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)

dat<-data.frame(x=dat$x0,y=dat$x1,z=dat$y)


b1<-gam(z~s(x,y),data=dat)
par(mfrow=c(2,2))
vis.gam(b1,plot.type="contour")

bnd.mds<-list(x=c(1,1,0,0,1),y=c(1,0,0,1,1))


#b2<-gam(z~s(x,y,bs="mdstp",xt=list(bnd.mds=bnd.mds,
#                                     bnd=bnd.mds,
#                                     op=NULL,
#                                     mds.obj=NULL
#         )),data=dat)
#
#vis.gam(b2,plot.type="contour")



source("makesoapgrid.R")

sg<-make_soap_grid(bnd.mds,c(10,10))
D.grid<-create_distance_matrix(sg$x,sg$y,bnd.mds,faster=0)

grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)


source("intexp/smooth2.c.R")

b3<-gam(z~s(x,y,bs="mdstp",xt=list(bnd.mds=bnd.mds,
                                     bnd=bnd.mds,
                                     op=sg,
                                     b.grid=c(50,50),
                                     mds.obj=grid.mds
         )),data=dat)

vis.gam(b3,plot.type="contour")

source("intexp/smoothKDE.R")

b4<-gam(z~s(x,y,bs="mdstp",xt=list(bnd.mds=bnd.mds,
                                     bnd=bnd.mds,
                                     op=sg,
                                     b.grid=c(50,50),
                                     mds.obj=grid.mds
         )),data=dat)

vis.gam(b4,plot.type="contour")
