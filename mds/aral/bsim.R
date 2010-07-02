# do large scale simulations for the Aral sea
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
sim.size<-200
n.samp<-250
# noise levels = 0.35,0.9,1.55
# snr = 0.95,0.75,0.50
snrs<-c(0.95,0.75,0.50)
samp.size<-c(100,250,500)
#noise.level<-1.55

# model names
#modnames<-c("tprs","mdstp","mdscr","mdstp3d","soap")#,"mdstpadj","soap")
modnames<-c("tprs","mdstp","mdscr","mdstp3d","mdstpadj","soap")
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
#npmodbw<-npregbw(formula=chl~x+y,regtype = "ll",bwmethod = "cv.aic",data =aral.dat,bwtype="adaptive_nn")
#npmod<-npreg(npmodbw)


#npmod<-npreg(txdat=cbind(aral.dat$x,aral.dat$y),tydat=aral.dat$chl,regtype = "ll",bwmethod = "cv.aic",bws=c(12,2),bwtype="adaptive_nn",exdat=cbind(pred.points$x,pred.points$y))
#npmod<-npreg(txdat=cbind(aral.dat$x,aral.dat$y),tydat=aral.dat$chl,regtype = "ll",bwmethod = "cv.aic",bwtype="adaptive_nn",exdat=cbind(pred.points$x,pred.points$y))
#npmod<-npreg(npmodbw)

# two halves
ind<-aral.dat$x< -45
ind2<-pred.points$x< -45
npmod1<-npreg(txdat=cbind(aral.dat$x[ind],aral.dat$y[ind]),tydat=aral.dat$chl[ind],regtype = "ll",bwmethod = "cv.aic",bwtype="adaptive_nn",exdat=cbind(pred.points$x[ind2],pred.points$y[ind2]))

npmod2<-npreg(txdat=cbind(aral.dat$x,aral.dat$y),tydat=aral.dat$chl,regtype = "ll",bwmethod = "cv.aic",bwtype="adaptive_nn",exdat=cbind(pred.points$x,pred.points$y))

new.truth<-rep(0,length(pred.points$x))
new.truth<-npmod2$mean
new.truth[ind2]<-npmod1$mean


#new.truth<-predict(npmod,data=aral.dat,newdata=pred.points)
#new.truth<-predict(npmod,pred.points)
#new.truth<-npmod$mean
## taking a summary() of this...
#> summary(new.truth)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    M
# 2.423   4.790   7.032   7.074   8.678  13.980 

if(plot.it==TRUE){ 
#   par(mfrow=c(2,2)) 
   pdf("aral-np.pdf",width=2.5,height=3)
   par(las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

   pred.mat<-make_soap_grid(bnd,pred.n,mat=T)$mat 
   pred.mat[!is.na(pred.mat)]<-new.truth 
#   pind<-inSide(bnd,pred.points$x,pred.points$y)
#   pred.mat<-matrix(NA,50,50)
#   pred.mat[pind]<-new.truth
   zlims<-c(1.905461, 19.275249)

   image(z=pred.mat,x=seq(min(bnd$x),max(bnd$x),len=50),
                    y=seq(min(bnd$y),max(bnd$y),len=50),
         xlab="km (East)",ylab="km (North)",
         main="",asp=1,zlim=zlims) 
   contour(z=pred.mat,add=TRUE,zlim=c(0,20),
           x=seq(min(bnd$x),max(bnd$x),len=50),
           y=seq(min(bnd$y),max(bnd$y),len=50),asp=1,
           levels=pretty(zlims,15))
   lines(bnd,lwd=2)
   dev.off()
} 


# pre-calculate the MDS base configuration
mds.grid<-make_soap_grid(bnd,15)
D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd,faster=1)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)
grid.mds3<-cmdscale(D,eig=TRUE,k=3,x.ret=TRUE)
# prediction data for mds
pred.mds<-insert.mds(pred.points,mds.grid,grid.mds,bnd)
pred.mds<-list(x=pred.mds[,1],y=pred.mds[,2],chl=new.truth)
pred.mds3<-insert.mds(pred.points,mds.grid,grid.mds3,bnd)
pred.mds3<-list(x=pred.mds3[,1],y=pred.mds3[,2],z=pred.mds3[,3],chl=new.truth)


# SNR = 0.95, 0.75, 0.5
disp<-c(0.15,0.79,1.05)
eta<-c(1,1.15,1.7)

for(i.s in 1:3){
   for(i.n in 1:3){
   # actually run the sim
   source("aral/sim.R")
   }
}

