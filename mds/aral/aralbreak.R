
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
#tst<-list(x=c(-90.138483,15.789678),
#          y=c(-78.331649,135.697684))

#tst<-list(x=c(-47.767218,-5.395954),
#          y=c(-24.824316,135.697684))
#plot(bnd,type="l")
#path<-list(x=c(),y=c())
#path$x<-c(path$x,-90.138483)
#path$y<-c(path$y,15.306184)
#path$x<-c(path$x,-46.146022)
#path$y<-c(path$y,-40.248430)
#path$x<-c(path$x,-46.537547)
#path$y<-c(path$y,-21.805957)
#path$x<-c(path$x,-52.598789)
#path$y<-c(path$y,-21.805957)
#path$x<-c(path$x,-59.195151)
#path$y<-c(path$y,38.755778)
#path$x<-c(path$x,-41.021036)
#path$y<-c(path$y,63.601618)
#path$x<-c(path$x,-26.836254)
#path$y<-c(path$y,75.248106)
#path$x<-c(path$x,-18.731849)
#path$y<-c(path$y,63.601618)
#path$x<-c(path$x,-11.732498)
#path$y<-c(path$y,27.109291)
#path$x<-c(path$x,5.196862)
#path$y<-c(path$y,-105.085315)
#lines(path,lwd=2,col="red")

tst<-list(x=c(-90.138483,5.196862),
          y=c(15.306184,-105.085315))

#tst<-pe(mds.grid,c(14,243))
#tst<-pe(mds.grid,c(14,244))
#tst<-pe(mds.grid,c(13,245))

Dbug<-create_distance_matrix(tst$x,tst$y,bnd)



