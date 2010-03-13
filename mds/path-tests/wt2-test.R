options(echo=FALSE)
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


x<-my.grid$x
y<-my.grid$y



D<-create_distance_matrix(x,y,bnd)

# "true" D
D.t<-read.csv("path-tests/wt2-D.csv")

D.t<-as.matrix(D.t)
D.t<-D.t[,2:49]

if(max(abs(D-D.t))>(.Machine$double.eps)){
   cat("# Uh oh!",max(abs(D-D.t))," not the same as \"truth\"\n")
}
options(echo=TRUE)
