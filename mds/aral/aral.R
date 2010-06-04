# Aral Sea data set analysis

# run from phd-smoothing/mds


# libraries
library(mgcv)
library(soap)

# mds files
source("mds.R")
source("createrefgrid.R")
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
                     chl=as.numeric(aral$chl[onoff]))

# convert boundary to northings and eastings
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)
bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)

######################################################################
# plot setup
par(mfrow=c(2,2))

# set the x and y values for the image plot
aral.lab<-latlong2km(unique(sort(aral$lo)),unique(sort(aral$la)),59.5,45)
# set the plot limits
xlims<-c(min(aral.dat$x)-10,max(aral.dat$x)+10)
ylims<-c(min(aral.dat$y)-10,max(aral.dat$y)+10)

######################################################################
#### plot some raw data

aral$chl[!onoff]<-NA
image(z=matrix(aral$chl,46,46),x=aral.lab$km.e,y=aral.lab$km.n,
      asp=1,main="raw data",xlab="km (East)",ylab="km (North)")
lines(bnd,lwd=2)

######################################################################
#### fit a thin plate model
tp.fit<-gam(chl~s(x,y,k=49),data=aral.dat,family=Gamma(link="log"))

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
lines(bnd,lwd=2)


######################################################################
#### MDS
# mds grid
m<-20;n<-20
xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
grid.onoff<-inSide(bnd,xx,yy)
mds.grid<-data.frame(x=xx[grid.onoff],y=yy[grid.onoff])

# actually do the MDS
D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd,faster=1)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)

# create the data frame and fit the model
aral.mds<-insert.mds(aral.dat,mds.grid,grid.mds,bnd,faster=1)
aral.mds<-data.frame(x=aral.mds[,1],
                     y=aral.mds[,2],
                     chl=aral.dat$chl)

# fit the model
mds.fit<-gam(chl~s(x,y,k=49),data=aral.mds,family=Gamma(link="log"))

# mds prediction grid
pred.grid.mds<-insert.mds(pred.grid,mds.grid,grid.mds,bnd,faster=1)
pred.grid.mds<-data.frame(x=pred.grid.mds[,1],
                          y=pred.grid.mds[,2])

# do the prediction
mds.pred<-predict(mds.fit,newdata=pred.grid.mds)

# plot
pred.mat<-matrix(NA,m,n)
pred.mat[pred.onoff]<-mds.pred
image(pred.mat,x=unique(xx),y=unique(yy),main="mds",xlab="km (East)",ylab="km (North)")
lines(bnd,lwd=2)


######################################################################
#### soap 

### VVVVVVVVVVVVVVVVVVV this doesn't work (?!)
# first setup the knots
#s.knots<-create_refgrid(bnd,140)
#s.knots$nrefx<-NULL
#s.knots$nrefy<-NULL
#s.knots<-as.data.frame(s.knots)

#s.knots<-make_soap_grid(bnd,17)
s.knots<-make_soap_grid(bnd,c(12,12))

soap.fit<-gam(chl~s(x,y,k=49,bs="so",xt=list(bnd=list(bnd))),knots=s.knots,
            family=Gamma(link="log"),data=aral.dat)

# prediction
soap.pred<-predict(soap.fit,newdata=pred.grid)
pred.mat<-matrix(NA,m,n)
pred.mat[pred.onoff]<-soap.pred
image(pred.mat,x=unique(xx),y=unique(yy),main="soap")


