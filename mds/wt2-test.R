# load soap
library(soap)

## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE) 
names(bnd)<-c("x","y")

bnd$x<--bnd$x
bnd$y<--bnd$y

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
#x<-c(-2.4688416, -1.1883870, -2.0504753,  0.0540343,  0.7386338,  1.7021442,  2.7163657)
#y<-c(2.1441334, 1.8962821, -1.8345328, -2.4867731, -0.6213657, -1.0127099, -2.5519972)

# upside down fail
x=c( -1.421042, 2.043579 )
y=c( 1.359499, -2.459011 )

# another
x=c( -1.534870, 2.043579 )
y=c( 1.318058, -2.459011 )

# error in on_line
#Error in if (abs(leftside - rightside) < eps) { : 
#  missing value where TRUE/FALSE needed
#x=c( -0.6690523, 2.043579 )
#y=c( 2.793287, -2.459011 )




D<-create_distance_matrix(x,y,bnd)

