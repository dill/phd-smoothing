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
#ind<-c(10,116)
#ind<-c(18,79)
#ind<-c( 79, 72, 73, 74, 75, 76, 77, 68, 69, 70, 71, 18)
#ind<-c(116,79,18, 9,10,11,12,21,22,23,33,34,35,36,45,46,47,48,57, 58,59,60,68,69,70,71,78,86,87,94,95,96,104,105)



x<-my.grid$x[ind]
y<-my.grid$y[ind]

Dbug<-create_distance_matrix(x,y,bnd,faster=1)
