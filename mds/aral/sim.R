# Aral Sea data set simulation
# IDEA: fit soap to the Aral sea data set then predict over the
# whole area. Add noise to that prediction over a simulation.

# run from phd-smoothing/mds

#### OPTIONS!

plot.it=TRUE #FALSE # should we do some plotting?
n.sim<-1 # size of the simulation  (!!!! Set this to 1 if the above is TRUE)
n.samp<-250 # number of samples per run
noise.level<-0.5 # noise
#################


# libraries
library(mgcv)
library(soap)
library(np)

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
s.grid<-as.data.frame(make_soap_grid(bnd,10))

s.grid<-pe(s.grid,-2)

#b.soap<-gam(chl~s(x,y,k=80,bs="so",xt=list(bnd=list(bnd))),knots=s.grid,
#            family=Gamma(link="log"),data=aral.dat)

# now predict over the domain, this is now "truth"
pred.n<-50 # prediction grid size

# create the prediction points
pred.points<-make_soap_grid(bnd,pred.n)

# make the soap prediction
#new.truth<-predict(b.soap,newdata=pred.points)
# soap plots
#if(plot.it==TRUE){
#   par(mfrow=c(2,2))
#   pred.mat<-make_soap_grid(bnd,pred.n,mat=T)$mat
#   pred.mat[!is.na(pred.mat)]<-new.truth
#   image(pred.mat)
#}

## taking a summary() of this...
#> summary(new.truth)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    M
# 0.8798  1.5230  1.9360  1.8720  2.1690  2.7

## using np to create the "truth"
npmodbw<-npregbw(formula=chl~x+y,regtype = "ll",bwmethod = "cv.aic",data =aral.dat,bwtype="generalized_nn")
npmod<-npreg(npmodbw)
new.truth<-predict(npmod,data=aral.dat,newdata=pred.points)


if(plot.it==TRUE){
   par(mfrow=c(2,2))
   pred.mat<-make_soap_grid(bnd,pred.n,mat=T)$mat
   pred.mat[!is.na(pred.mat)]<-new.truth
   image(pred.mat,main="data from np")
}



# pre-calculate the MDS base configuration
mds.grid<-make_soap_grid(bnd,15)
D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd,faster=1)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)

# storage for the MSEs and EDFs
mses<-matrix(0,n.sim,3)
edfs<-matrix(0,n.sim,3)

# prediction data for mds
pred.mds<-insert.mds(pred.points,mds.grid,grid.mds,bnd)
pred.mds<-list(x=pred.mds[,1],y=pred.mds[,2],chl=new.truth)

for(i in 1:n.sim){

   ## take the sample
   samp.ind<-sample(1:length(new.truth),n.samp)

   # add some noise
#   disp <- 0.2 # , 0.4, 0.6# , stnr corr between y and \hat{y}
#   g    <- exp(eta)
#   y    <- rgamma( rep(1,n) , shape=1/disp,  scale=1/ (1/ (disp*g) ) )

   #noise<- rgamma(n.samp,2,3.5)
   noise<-rep(0,n.samp)

   samp<-data.frame(x=pred.points$x[samp.ind],
                    y=pred.points$y[samp.ind],
                    chl=as.numeric(new.truth[samp.ind]+noise))
  
   ### fit some models
   # tprs
   tp.fit<-gam(chl~s(x,y,k=80),data=samp,family=Gamma(link="log"))

   # soap
   soap.fit<-gam(chl~s(x,y,k=40,bs="so",xt=list(bnd=list(bnd))),knots=s.grid,
            family=Gamma(link="log"),data=samp)

   # MDS
   samp.mds<-insert.mds(samp,mds.grid,grid.mds,bnd)
   samp.mds<-list(x=samp.mds[,1],y=samp.mds[,2],chl=samp$chl)
   mds.fit<-gam(chl~s(x,y,k=80),data=samp.mds,family=Gamma(link="log"))


   ### do some prediction
   tp.pred<-predict(tp.fit,newdata=pred.points,type="response")
   soap.pred<-predict(soap.fit,newdata=pred.points,type="response")
   mds.pred<-predict(mds.fit,newdata=pred.mds,type="response")

   if(plot.it==TRUE){

      pred.mat[!is.na(pred.mat)]<-tp.pred
      image(pred.mat,main="tprs")

      pred.mat[!is.na(pred.mat)]<-soap.pred
      image(pred.mat,main="soap")

      pred.mat[!is.na(pred.mat)]<-mds.pred
      image(pred.mat,main="mds")

   }


   # calculate the MSE
   mses[i,1]<-mean((tp.pred-new.truth)^2,na.rm=T)
   mses[i,2]<-mean((soap.pred-new.truth)^2,na.rm=T)
   mses[i,3]<-mean((mds.pred-new.truth)^2,na.rm=T)

   # calculate the EDFs
   edfs[i,1]<-sum(tp.fit$edf)
   edfs[i,2]<-sum(soap.fit$edf)
   edfs[i,3]<-sum(mds.fit$edf)
}
