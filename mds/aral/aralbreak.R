
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

# this dies
#D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd)

# these points seem to be responsible
#p1=(-90.138483,-78.331649)
#p2=(15.789678,135.697684)
tst<-list(x=c(-90.138483,15.789678),
          y=c(-78.331649,135.697684))


#tst<-pe(mds.grid,c(14,243))
#tst<-pe(mds.grid,c(14,244))
#tst<-pe(mds.grid,c(13,245))

Dbug<-create_distance_matrix(tst$x,tst$y,bnd)



