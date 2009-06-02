# load soap
library(soap)

## create a boundary...
fsb <- fs.boundary()

## Simulate some fitting data, inside boundary...
n<-50
x <- runif(n)*5-1; y<-runif(n)*2-1
z <- fs.test(x,y,b=1)
ind <- inSide(fsb,x=x,y=y) ## remove outsiders
x<-x[ind];y <- y[ind];z <- z[ind]
n <- length(z)
z <- z + rnorm(n)*.3 ## add noise


source("mds.R")
source("wood.R")

# instead, creating a set of test points
#x<-c(1.4784634,1.0423811,2.0319526,3.1473170,3.1137722,-0.5845415,-0.6767896)
#y<-c(0.4050137,-0.5244773,-0.5244773,-0.5663463,0.5976109,0.2375378,-0.4574870)

D<-create_distance_matrix(x,y,fsb)
