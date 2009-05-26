# load soap
library(soap)

## create a boundary...
bnd <- read.csv("doubleyuh-verts.csv",header=FALSE) 
names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
n<-100
x <- runif(n)*6; y<-runif(n)*4
ind <- inSide(bnd,x=x,y=y) ## remove outsiders
x<-x[ind];y <- y[ind];#z <- z[ind]
n <- length(x)

plot(bnd,type="l")
points(x=x,y=y,pch=".")
a<-scan()

source("mds.R")
source("wood.R")
source("utils.R")

# instead, creating a set of test points

D<-create_distance_matrix(x,y,bnd)

