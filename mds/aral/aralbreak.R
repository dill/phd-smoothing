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


x<-c(-40.624116,49.107410)
y<-c(115.955353,134.811571)


x<-c(79.017919049535479,-100.44513318120238 )
y<-c(-53.750608069735968, -16.038172315109264)

x<-c(4.241647,-100.445133)
y<-c(134.81157,-72.60683)


x<-c(4.241647,-55.579370)
y<-c(134.811571,-34.894390)

x<-my.grid$x[c(34,114)]
y<-my.grid$y[c(34,114)]

Dbug<-create_distance_matrix(x,y,bnd)
