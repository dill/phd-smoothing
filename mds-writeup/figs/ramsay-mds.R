# diagram for MDS on the Ramsay horseshoe 
# Copyright David Lawrence Miller 2009
source("mds.R")

## create a boundary...
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
# create points within the boundary 
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
onoff.fix<-inSide(list(x=-bnd$x,y=-bnd$y),-xx,-yy)
xx<-xx[onoff.fix];yy<-yy[onoff.fix]
   

# find the distances 
D<-create_distance_matrix(xx,yy,bnd)

# perform the MDS
mds<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=2)


### 2D plot
par(mfrow=c(1,2))

# before MDS
plot(xx,yy,asp=1,main="",xlab="x",ylab="y",pch=19,cex=0.2)
lines(fs.boundary())

# after MDS
plot(mds$points[,1],mds$points[,2],asp=1,main="",xlab=expression(tilde(x)),ylab=expression(tilde(y)),pch=19,cex=0.2)



### for 3D

mds3<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=3)
par(mfrow=c(1,3))
plot(mds3$points[,1],mds3$points[,2],asp=1,xlab=expression(tilde(x)),ylab=expression(tilde(y)),pch=19,cex=0.3)
plot(mds3$points[,1],mds3$points[,3],asp=1,xlab=expression(tilde(x)),ylab=expression(tilde(z)),pch=19,cex=0.3)
plot(mds3$points[,2],mds3$points[,3],asp=1,xlab=expression(tilde(y)),ylab=expression(tilde(z)),pch=19,cex=0.3)






