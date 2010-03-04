# Aral Sea data set simulation
# IDEA: fit soap to the Aral sea data set then predict over the
# whole area. Add noise to that prediction over a simulation.

# run from phd-smoothing/mds

# libraries
library(mgcv)
library(soap)

# mds files
source("mds.R")
source("latlong2km.R")
source("makesoapgrid.R")

# load the data and boundary
aral<-read.csv("aral/aral.dat",sep=" ")
bnd<-read.csv("aral/aralbnd.csv")

# clean up the data
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


### First fit the soap

# create prediction grid for soap
s.grid<-make_soap_grid(bnd,c(10,10))

s.grid<-pe(s.grid,-2)

b.soap<-gam(chl~s(x,y,k=49,bs="so",xt=list(bnd=list(bnd))),knots=s.grid,
            family=Gamma(link="log"),data=aral.dat)

# now predict over the domain, this is now "truth"





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










###### OTHER MODELS

#### fit a thin plate model
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
#image(pred.mat,x=unique(xx),y=unique(yy),main="tprs",xlab="km (East)",ylab="km (North)")
#
##### MDS
## mds grid
#m<-20;n<-20
#xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
#yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
#xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
#grid.onoff<-inSide(bnd,xx,yy)
#mds.grid<-data.frame(x=xx[grid.onoff],y=yy[grid.onoff])
#
## actually do the MDS
#D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd)
#grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)
#
## create the data frame and fit the model
#aral.mds<-insert.mds(aral.dat,mds.grid,grid.mds,bnd)
#aral.mds<-data.frame(x=aral.mds[,1],
#                     y=aral.mds[,2],
#                     chl=aral.dat$chl)
#
#
#mds.fit<-gam(chl~s(x,y,k=49),data=aral.mds)
#
## prediction grid
#m<-50;n<-50
#xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
#yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
#xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
#pred.onoff<-inSide(bnd,xx,yy)
#pred.grid<-data.frame(x=xx[pred.onoff],y=yy[pred.onoff])
#pred.grid.mds<-insert.mds(pred.grid,mds.grid,grid.mds,bnd)
#
#pred.grid.mds<-data.frame(x=pred.grid.mds[,1],
#                          y=pred.grid.mds[,2])
#mds.pred<-predict(mds.fit,newdata=pred.grid.mds)
#pred.mat<-matrix(NA,m,n)
#pred.mat[pred.onoff]<-mds.pred
#image(pred.mat,x=unique(xx),y=unique(yy),main="mds",xlab="km (East)",ylab="km (North)")

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











