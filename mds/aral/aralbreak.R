# libraries
library(mgcv)
library(soap)

# mds files
source("mds.R")
source("latlong2km.R")

# load the data and boundary
aral<-read.csv("aral/aral.dat",sep=" ")
bnd<-read.csv("aral/aralbnd.csv")

# first cut out the crap using inSide
onoff<-inSide(bnd,aral$lo,aral$la)

# converstion to km
aral.km<-latlong2km(aral$lo[onoff],aral$la[onoff],59.5,45)

aral.dat<-data.frame(x=aral.km$km.e,
                     y=aral.km$km.n,
                     chl=aral$chl[onoff])

# convert boundary to northings and eastings
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)

bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)
aral$chl[!onoff]<-NA
m<-20;n<-20
xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
grid.onoff<-inSide(bnd,xx,yy)
mds.grid<-data.frame(x=xx[grid.onoff],y=yy[grid.onoff])

x<-c(-40.624116,49.107410)
y<-c(115.955353,134.811571)


x<-c(79.017919049535479,-100.44513318120238 )
y<-c(-53.750608069735968, -16.038172315109264)

x<-c(4.241647,-100.445133)
y<-c(134.81157,-72.60683)


x<-c(4.241647,-55.579370)
y<-c(134.811571,-34.894390)


Dbug<-create_distance_matrix(x,y,bnd)
