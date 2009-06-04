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

## Simulate some fitting data, inside boundary...
n<-50
x <- runif(n)*6-3; y<-runif(n)*6-3
#z <- fs.test(x,y,b=1)
ind <- inSide(bnd,x=x,y=y) ## remove outsiders
x<-x[ind];y <- y[ind];#z <- z[ind]
n <- length(x)
#z <- z + rnorm(n)*.3 ## add noise


source("mds.R")
source("wood.R")
source("utils.R")

# instead, creating a set of test points
x<-c(-2.4688416, -1.1883870, -2.0504753,  0.0540343,  0.7386338,  1.7021442,  2.7163657,-2.336735 , -1.806122)
y<-c(2.1441334, 1.8962821, -1.8345328, -2.4867731, -0.6213657, -1.0127099, -2.5519972, -0.06122449 , -3)

#x=c( -1.010204 , 0.1836735 )
#y=c( -2.510204 , -3 )
#
#x=c( -0.08163265 , 1.112245 )
#y=c( -3 , -2.387755 )


# rotation
#tmp<-x
#x<-y
#y<-tmp


D<-create_distance_matrix(x,y,bnd)

#newcoords<-cmdscale(D)

