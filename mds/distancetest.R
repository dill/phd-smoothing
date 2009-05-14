# load soap
library(soap)

## create a boundary...
fsb <- fs.boundary()

## Simulate some fitting data, inside boundary...
n<-100
x <- runif(n)*5-1; y<-runif(n)*2-1
z <- fs.test(x,y,b=1)
ind <- inSide(fsb,x=x,y=y) ## remove outsiders
x<-x[ind];y <- y[ind];z <- z[ind]
n <- length(z)
z <- z + rnorm(n)*.3 ## add noise


source("mds.R")
source("convexhull.R")


D<-create_distance_matrix(x,y,fsb)
