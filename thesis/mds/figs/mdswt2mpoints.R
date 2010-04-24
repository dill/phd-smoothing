# Copyright David Lawrence Miller 2010
# diagram of the mapping of wt2 into MDS space.

# NB RUN from mds directory

source("mds.R")
 
## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

 
# create the grid
my.grid<-create_refgrid(bnd,500)



## do the MDS on the grid 
# create D
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)

# perform mds on D
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
 
# do the plotting
pdf("../thesis/mds/figs/mdswt2points.pdf",6,3)
par(mfrow=c(1,2))
plot(my.grid,asp=1,cex=0.3,cex.axis=0.5,pch=19,xlab="",ylab="")
plot(grid.mds$points,asp=1,cex=0.3,cex.axis=0.5,pch=19,xlab="",ylab="")
dev.off()

