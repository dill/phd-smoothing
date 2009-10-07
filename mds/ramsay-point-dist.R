# See what happens when we only pick points in one arm

# load soap
library(soap)
source("mds.R")


## create a boundary...
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))


# fitting grid 
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
onoff.fix<-inSide(list(x=-bnd$x,y=-bnd$y),-xx,-yy)
x<-xx[onoff.fix];y<-yy[onoff.fix]
n<-length(xx)


### take points from one arm
onoff<-x>0 & y<0
arm<-list(x=x[onoff],y=y[onoff])
# distances
D<-create_distance_matrix(arm$x,arm$y,bnd)
# mds
arm.cmd<-cmdscale(D,eig=T)


### the rest of the points
rest<-list(x=x[!onoff],y=y[!onoff])
new.cmd<-insert.mds(rest,arm,arm.cmd,bnd)



# plot



