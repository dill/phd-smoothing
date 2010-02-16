options(echo=FALSE)
# load soap
library(soap)
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

x<-my.grid$x
y<-my.grid$y

source("mds.R")


D<-create_distance_matrix(x,y,bnd)

newcoords<-cmdscale(D)

options(echo=TRUE)
