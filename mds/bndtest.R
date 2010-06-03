# how does the boundary map? 
# Copyright David Lawrence Miller 2010.
source("mds.R")
 
par(mfrow=c(1,3))

## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")
   
# create the grid
my.grid<-create_refgrid(bnd,300)

plot(bnd,type="l",lwd=2,asp=1)
points(my.grid,pch=19,cex=0.3)

## do the MDS on the grid 
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)

# plot those points
plot(grid.mds$points,pch=19,cex=0.3,asp=1)

# map the boundary 
bnd.mds<-insert.mds(bnd,my.grid,grid.mds,bnd,faster=0)
bnd.mds<-data.frame(x=bnd.mds[,1],y=bnd.mds[,2])



plot(grid.mds$points,asp=1,pch=19,cex=0.3)
lines(bnd.mds,lwd=2)

# interpolate the boundary
# weird things happen here
xb<-matrix(c(bnd$x,bnd$x[c(2:length(bnd$x),1)]),length(bnd$x),2)[-length(bnd$x),] 
xb<-xb[-dim(xb),]
yb<-matrix(c(bnd$y,bnd$y[c(2:length(bnd$y),1)]),length(bnd$y),2)[-length(bnd$y),] 
yb<-yb[-dim(yb),]
int.bnd<-list(x=vecseq(xb,10),
              y=vecseq(yb,10))

ibnd.mds<-insert.mds(int.bnd,my.grid,grid.mds,bnd,faster=0)
ibnd.mds<-data.frame(x=ibnd.mds[,1],y=ibnd.mds[,2])

plot(grid.mds$points,asp=1,pch=19,cex=0.3)
lines(ibnd.mds,lwd=2)
