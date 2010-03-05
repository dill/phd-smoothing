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
s.grid<-make_soap_grid(bnd,10)

s.grid<-pe(s.grid,-2)

b.soap<-gam(chl~s(x,y,k=80,bs="so",xt=list(bnd=list(bnd))),knots=s.grid,
            family=Gamma(link="log"),data=aral.dat)

# now predict over the domain, this is now "truth"
pred.n<-80 # prediction grid size

# create the prediction points
pred.points<-make_soap_grid(bnd,pred.n)

# make the soap prediction
new.truth<-predict(b.soap,newdata=pred.points)

# plots
#pred.mat<-make_soap_grid(bnd,pred.n,mat=T)$mat
#pred.mat[!is.na(pred.mat)]<-new.truth
#image(pred.mat)


# pre-calculate the MDS base configuration
mds.grid<-make_soap_grid(bnd,15)
D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)

n.sim<-1
n.samp<-100
noise.level<-0.5

mses<-matrix(0,n.sim,3)

for(i in 1:n.sim){

   ## take the sample and add some noise
   samp.ind<-sample(1:length(new.truth),n.samp)
   noise<-rnorm(n.samp)*noise.level
   samp<-data.frame(x=pred.points$x[samp.ind],
                    y=pred.points$y[samp.ind],
                    chl=new.truth[samp.ind]+noise)

  
   ### fit some models
   # tprs
   tp.fit<-gam(chl~s(x,y,k=49),data=samp,family=Gamma(link="log"))

   # soap
   soap.fit<-gam(chl~s(x,y,k=80,bs="so",xt=list(bnd=list(bnd))),knots=s.grid,
            family=Gamma(link="log"),data=samp)

   # MDS
   samp.mds<-insert.mds(samp,mds.grid,grid.mds,bnd)
   samp.mds$chl<-samp$chl[samp.ind]
   mds.fit<-gam(chl~s(x,y,k=49),data=samp.mds,family=Gamma(link="log"))


   ### do some prediction
   tp.pred<-predict(tp.fit,newdata=new.truth)
   soap.pred<-predict(soap.fit,newdata=new.truth)

   pred.mds<-insert.mds(new.truth,mds.grid,grid.mds,bnd)
   mds.pred<-predict(mds.fit,newdata=pred.mds)


   # calculate the MSE
   mses[i,1]<-mean((tp.pred.mapped-new.truth)^2,na.rm=T)
   mses[i,2]<-mean((soap.pred-new.truth)^2,na.rm=T)
   mses[i,3]<-mean((mds.pred-new.truth)^2,na.rm=T)

}
