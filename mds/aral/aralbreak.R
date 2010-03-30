# libraries
library(mgcv)
library(soap)

# mds files
source("mds.R")
source("latlong2km.R")

# load the data and boundary
aral<-read.csv("aral/aral.dat",sep=" ")
bnd<-read.csv("aral/aralbnd.csv")

# convert boundary to northings and eastings
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)

bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)

my.grid<-make_soap_grid(bnd,15)

ind<-c(9,116)

ind<-c(46,50)
ind<-c(74,104)


x<-my.grid$x[ind]
y<-my.grid$y[ind]

Dbug<-create_distance_matrix(x,y,bnd,faster=1)
