# load soap
library(soap)
source("mds.R")
## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE) 
names(bnd)<-c("x","y")

# upside down
#bnd$x<--bnd$x
#bnd$y<--bnd$y

# rotation
#tmp<-bnd$x
#bnd$x<-bnd$y
#bnd$y<-tmp

# make a grid to do the fitting over
source("makesoapgrid.R")
my.grid<-make_soap_grid(bnd,10)

#my.grid<-pe(my.grid,-which(my.grid$y==min(bnd$y)))
#my.grid<-pe(my.grid,-which(my.grid$x==max(bnd$x)))


#x<-c(-0.771875,-2.701563)
#y<-c( 1.425,-2.850)
#x<-c(-2.527778,-1.083333)
#y<-c(1.000000,1.000000)
#x<-c(2.816667,1.5)
#y<-c(-3,0.2002)
x<-c(-1.895833,-1.472222,-1.416667)
y<-c(2.750000,1.666667,2)

#D<-create_distance_matrix(x,y,bnd,faster=1)

my.grid<-create_refgrid(bnd,300)
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=1)
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
samp.mds<-insert.mds(list(x=x,y=y),my.grid,grid.mds,bnd,faster=1)


# do the MDS on the grid 
#my.grid<-create_refgrid(bnd,120)
#D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)
#
## perform mds on D
#grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
#
## make the line
## between vertex 21 and 22
#xa<- -2.5; xb<- -2.5
#ya<-0.5; yb<-1.5
#
#x<-seq(xa,xb,len=25)
#y<-seq(ya,yb,len=25)
#
#dat<-list(x=x,y=y)
#
## sample points insertion
#samp.mds<-insert.mds(dat,my.grid,grid.mds,bnd,faster=1)
#
#par(mfrow=c(1,2))
#plot(bnd,type="l")
#points(my.grid,pch=19,cex=0.3)
#points(x=x,y=y,pch=19,col="red")
#
#plot(grid.mds$points,pch=19,cex=0.3)
#points(samp.mds,pch=19,col="red")




