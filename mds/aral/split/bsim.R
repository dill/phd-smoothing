# do large scale simulations for the Aral sea
# split aral into <-45 and >=45, record MSE for each half.
# should be higher on the small peninsulae
# Copyright David Lawrence Miller 2010.

set.seed(1)

# libraries
library(mgcv)
library(soap)
library(np)

source("mds.R")
source("intexp/smooth2.c.R")
source("latlong2km.R")
source("makesoapgrid.R")

######################################################
# The following models get tested:
#
#     * tprs
#     * mds+tp
#     * mds+cr
#     * mds+tp (3d)
#     * mds+tp + penalty adjustments
#     * soap
######################################################
# OPTIONS
plot.it<-FALSE
sim.size<-100
# noise levels = 0.35,0.9,1.55
# snr = 0.95,0.75,0.50
snrs<-c(0.95,0.75,0.50)
samp.size<-c(100,250,500)
#noise.level<-1.55

# model names
modnames<-c("tprs","tprsin","mdstp","mdstpin","soap","soapin")
######################################################
# PREAMBLE
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


# now predict over the domain, this is now "truth"
pred.n<-50 # prediction grid size
# create the prediction points
pred.points<-make_soap_grid(bnd,pred.n)
# create prediction grid for soap
s.grid<-as.data.frame(make_soap_grid(bnd,12))
s.grid<-pe(s.grid,-2)


## using np to create the "truth"
# two halves
ind<-aral.dat$x< -45
ind2<-pred.points$x< -45
# just the smaller lobe
npmod1<-npreg(txdat=cbind(aral.dat$x[ind],aral.dat$y[ind]),tydat=aral.dat$chl[ind],regtype = "ll",bwmethod = "cv.aic",bwtype="adaptive_nn",exdat=cbind(pred.points$x[ind2],pred.points$y[ind2]))
# full model
npmod2<-npreg(txdat=cbind(aral.dat$x,aral.dat$y),tydat=aral.dat$chl,regtype = "ll",bwmethod = "cv.aic",bwtype="adaptive_nn",exdat=cbind(pred.points$x,pred.points$y))

new.truth<-rep(0,length(pred.points$x))
new.truth<-npmod2$mean
new.truth[ind2]<-npmod1$mean


# pre-calculate the MDS base configuration
mds.grid<-make_soap_grid(bnd,15)
D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd,faster=1)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)
#grid.mds3<-cmdscale(D,eig=TRUE,k=3,x.ret=TRUE)
# prediction data for mds
pred.mds<-insert.mds(pred.points,mds.grid,grid.mds,bnd)
pred.mds<-list(x=pred.mds[,1],y=pred.mds[,2],chl=new.truth)
#pred.mds3<-insert.mds(pred.points,mds.grid,grid.mds3,bnd)
#pred.mds3<-list(x=pred.mds3[,1],y=pred.mds3[,2],z=pred.mds3[,3],chl=new.truth)


# SNR = 0.95, 0.75, 0.5
disp<-c(0.15,0.79,1.05)
eta<-c(1,1.15,1.7)

#for(i.s in 1:3){
#   for(i.n in 1:3){
# just try one level at the moment
i.s<-1 # 100 samples
i.n<-2 # SNR 0.75
   # actually run the sim
   source("aral/split/sim.R")
#   }
#}

