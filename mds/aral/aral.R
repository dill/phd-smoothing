# Aral Sea data set analysis

# run from phd-smoothing/mds


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


# first cut out the crap using inSide
onoff<-inSide(bnd,aral$lo,aral$la)


# converstion to km
aral.km<-latlong2km(aral$lo[onoff],aral$la[onoff],59.5,45)


aral.dat<-data.frame(x=aral.km$km.e,
                     y=aral.km$km.n,
                     chl=aral$chl[onoff])


# plot setup
par(mfrow=c(2,2))

# plot some raw data
aral$chl[!onoff]<-NA
image(x=unique(aral$la),y=unique(aral$lo),z=matrix(aral$chl,46,46),
      asp=1,main="raw data with boundary")
lines(bnd)

#### fit a thin plate model
tp.fit<-gam(chl~s(x,y,k=49),data=aral.dat)

# prediction grid
m<-50;n<-50
xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
pred.onoff<-inSide(bnd,xx,yy)
pred.grid<-data.frame(x=xx[pred.onoff],y=yy[pred.onoff])

tp.pred<-predict(tp.fit,newdata=pred.grid)
pred.mat<-matrix(NA,m,n)
pred.mat[pred.onoff]<-tp.pred
image(pred.mat,x=unique(xx),y=unique(yy),main="tprs",xlab="km (East)",ylab="km (North)")

#### MDS

D<-create_distance_matrix(aral.dat$x,aral.dat$y,bnd)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)


tp.fit<-gam(chl~s(x,y,k=49),data=aral.dat)

# prediction grid
   pred.mds<-insert.mds(pred.data,my.grid,grid.mds,bnd)



m<-50;n<-50
xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
pred.onoff<-inSide(bnd,xx,yy)
pred.grid<-data.frame(x=xx[pred.onoff],y=yy[pred.onoff])

tp.pred<-predict(tp.fit,newdata=pred.grid)
pred.mat<-matrix(NA,m,n)
pred.mat[pred.onoff]<-tp.pred
image(pred.mat,x=unique(xx),y=unique(yy),main="tprs",xlab="km (East)",ylab="km (North)")

#### soap 
#tp.fit<-gam(chl~s(x,y,k=49),data=aral.dat)
#
## prediction grid
#m<-50;n<-50
#xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
#yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
#xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
#pred.onoff<-inSide(bnd,xx,yy)
#pred.grid<-data.frame(x=xx[pred.onoff],y=yy[pred.onoff])
#
#tp.pred<-predict(tp.fit,newdata=pred.grid)
#pred.mat<-matrix(NA,m,n)
#pred.mat[pred.onoff]<-tp.pred
#image(pred.mat,x=unique(xx),y=unique(yy),main="tprs")











