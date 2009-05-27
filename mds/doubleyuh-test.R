# load soap
library(soap)

## create a boundary...
bnd <- read.csv("doubleyuh-verts.csv",header=FALSE) 
names(bnd)<-c("x","y")

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

# doesn't work!
#x=c( 2.137168, 5.333916 )
#y=c( 0.7399752, 2.946539 )
#####
#x=c( 1.439891, 3.971544 )
#y=c( 1.939660, 0.6649949 )


#plot(bnd,type="l",asp=1)
#points(x=x,y=y,pch=".")
#a<-scan()

source("mds.R")
source("wood.R")
source("utils.R")

# instead, creating a set of test points

D<-create_distance_matrix(x,y,bnd)

