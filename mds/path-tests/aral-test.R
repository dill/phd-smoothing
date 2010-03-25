options(echo=FALSE)
# libraries
library(mgcv)
library(soap)

# mds files
source("mds.R")
source("latlong2km.R")
source("makesoapgrid.R")

# load the data and boundary
bnd<-read.csv("aral/aralbnd.csv")


# convert boundary to northings and eastings
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)

bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)

my.grid<-make_soap_grid(bnd,15)

D<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)

# "true" D
D.t<-read.csv("path-tests/aral-D.csv")

D.t<-as.matrix(D.t)
D.t<-D.t[,2:119]

if(max(abs(D-D.t))>(.Machine$double.eps)){
   cat("# Uh oh!",max(abs(D-D.t))," not the same as \"truth\"\n")
}
options(echo=TRUE)
