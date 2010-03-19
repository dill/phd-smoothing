# kernel density smoothing adjustment

# load MASS for kde2d
library(MASS)

# grab the MDS code
source("mds.R")
# and the vertices for wt2
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

# create a grid
#grid<-make_soap_grid(bnd,23)
grid<-create_refgrid(bnd)

# MDS it
D<-create_distance_matrix(grid$x,grid$y,bnd)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)

plot(grid.mds$points,col="red",pch=19)

new.grid<-make_soap_grid(bnd,23)
new.mds<-insert.mds(new.grid,grid,grid.mds,bnd)

points(new.mds,pch=19)

# okay, now fit the kde
kdefit<-kde2d(x=grid.mds$points[,1],y=grid.mds$points[,2],n=50)



