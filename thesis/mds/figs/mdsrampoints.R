# Copyright David Lawrence Miller 2010
# create a diagram showing the Ramsay horseshoe in Euclidean and
# MDS space.

library(msg)

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


# map the grid xx,yy
my.grid<-list(x=xx,y=yy)
D.grid<-create_distance_matrix(xx,yy,bnd)
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)


pdf(file="mdsrampoints.pdf",width=6,height=2.5)
par(mfrow=c(1,2),las=1,cex.lab=0.7,cex.axis=0.7,mar=c(3,2.5,1,1),mgp=c(1.8,0.7,0))
plot(my.grid,asp=1,xlab="x",ylab="y",pch=19,cex=0.2)
plot(grid.mds$points,asp=1,xlab="x*",ylab="y*",pch=19,cex=0.2)
dev.off()

