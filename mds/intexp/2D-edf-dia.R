# 2d testing of the integration

#source("smooth2NA.c.R")
source("intexp/smooth2.c.R")
source("mds.R")

# from the example in gam()
library(mgcv)
library(soap)
set.seed(0) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)

dat<-data.frame(x=dat$x0,y=dat$x1,z=dat$y)

bnd.mds<-list(x=c(1,1,0,0,1),y=c(1,0,0,1,1))


source("makesoapgrid.R")

sg<-make_soap_grid(bnd.mds,c(10,10))
D.grid<-create_distance_matrix(sg$x,sg$y,bnd.mds,faster=0)

grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)





par(mfrow=c(3,2))

# EDFs 6, 12, 3
# smoothing parameters
sp1<-c(2.5,0.27,600000)
sp2<-c(4,0.365,1000000)

# set k overly big ie. 20
k<-20

# first set
b1<-gam(z~s(x,y,k=k),sp=sp1[1],data=dat)
vis.gam(b1,plot.type="contour",asp=1)

b2<-gam(z~s(x,y,k=k,bs="mdstp",xt=list(bnd.mds=bnd.mds,
                                     bnd=bnd.mds,
                                     op=sg,
                                     mds.obj=grid.mds
         )),sp=sp2[1],data=dat)
vis.gam(b2,plot.type="contour",asp=1)

cat("### 1\n")
cat("EDF should be 6\n")
cat("b1 EDF=",sum(b1$edf),"\n")
cat("b2 EDF=",sum(b2$edf),"\n")


# second set
b1<-gam(z~s(x,y,k=k),sp=sp1[2],data=dat)
vis.gam(b1,plot.type="contour",asp=1)

b2<-gam(z~s(x,y,k=k,bs="mdstp",xt=list(bnd.mds=bnd.mds,
                                     bnd=bnd.mds,
                                     op=sg,
                                     mds.obj=grid.mds
         )),sp=sp2[2],data=dat)
vis.gam(b2,plot.type="contour",asp=1)

cat("### 2\n")
cat("EDF should be 12\n")
cat("b1 EDF=",sum(b1$edf),"\n")
cat("b2 EDF=",sum(b2$edf),"\n")

# third set
b1<-gam(z~s(x,y,k=k),sp=sp1[3],data=dat)
vis.gam(b1,plot.type="contour",asp=1)

b2<-gam(z~s(x,y,k=k,bs="mdstp",xt=list(bnd.mds=bnd.mds,
                                     bnd=bnd.mds,
                                     op=sg,
                                     mds.obj=grid.mds
         )),sp=sp2[3],data=dat)
vis.gam(b2,plot.type="contour",asp=1)

cat("### 3\n")
cat("EDF should be 3\n")
cat("b1 EDF=",sum(b1$edf),"\n")
cat("b2 EDF=",sum(b2$edf),"\n")





