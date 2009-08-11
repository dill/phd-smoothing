# load soap
library(soap)

#library(debug)

## create a boundary...
bnd <- read.csv("doubleyuh-verts.csv",header=FALSE) 
names(bnd)<-c("x","y")

# upside down
#bnd$x<--bnd$x
#bnd$y<--bnd$y

# rotate
#tmp<-bnd$x
#bnd$x<-bnd$y
#bnd$y<-tmp

## Simulate some fitting data, inside boundary...
#n<-100
#x <- runif(n)*6; y<-runif(n)*4
#ind <- inSide(bnd,x=x,y=y) ## remove outsiders
#x<-x[ind];y <- y[ind];#z <- z[ind]
#n <- length(x)

x<-c(5.3339164,2.9417190,3.9715439,3.2099026,2.1371683,1.4398911,0.8069778,2.4375339,
     3.9929986,5.5913726)

y<-c(2.9465388,1.6504505,0.6649949,1.0934539,0.7399752,1.9396603,3.8355911,0.6864179,
     0.1722671,3.8570141)

# rotate
#tmp<-x
#x<-y
#y<-tmp

#x<-c(1.4398911,3.9929986)
#y<-c(1.9396603,0.1722671)

source("mds.R")
#source("wood.R")
source("utils.R")

#x=c(-1.439891,  -3.971544 )
#y=c( -1.939660,-0.6649949 )



D<-create_distance_matrix(x,y,bnd)

