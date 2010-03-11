# kernel density smoothing adjustment

# load MASS for kde2d
library(MASS)

# grab the MDS code
source("mds.R")
# and the vertices for wt2
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

# create a grid
grid<-make_soap_grid(bnd,17)

# MDS it
D<-create_distance_matrix(grid$x,grid$y,bnd)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)

plot(grid.mds$points)


# okay, now fit the kde
#kdefit<-kde2d(x=grid.mds$points[,1],y=grid.mds$points[,2],n=50)



