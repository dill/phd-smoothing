# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")
 
## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")
   
# create the grid
#my.grid<-create_refgrid(bnd,300)
bndc<-pe(bnd,-length(bnd$x))

bnd.tri<-as.matrix(triangulate(bndc))

plot(bnd,asp=1,type="l",lwd=2)

for(i in 1:dim(bnd.tri)[1]){
   lines(x=bnd.tri[i,c(1,3,5,1)],y=bnd.tri[i,c(2,4,6,2)],col="green")
}

## do the MDS on the grid 
# create D
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)

# perform mds on D
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
 

par(mfrow=c(1,2))

plot(my.grid,asp=1,pch=19,cex=0.3)
lines(bnd, lwd=2)
#points(pe(my.grid,125),pch=19,col="red",cex=0.5)
plot(grid.mds$points,asp=1,pch=19,cex=0.3)
#points(x=grid.mds$points[125,1],y=grid.mds$points[125,2],pch=19,col="red",cex=0.5)


