# do large scale simulations for wt2 - WITH DUCHON splines!!
# David Lawrence Miller 2011.

library(mdspack)

source("ramsay-smooth-test.R")

faster=1

###############################
# initial setup
## create a boundary...
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
# create points within the boundary 
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
onoff[c(143,279)]<-FALSE ### UGLY HACK
xx<-xx[onoff];yy<-yy[onoff]

# make the grid
my.grid<-make_soap_grid(bnd,12)
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=faster)
# 2D
grid.mds2<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
# 3D
grid.mds3<-cmdscale(D.grid,eig=TRUE,k=3,x.ret=TRUE)

#################################

sim.size<-200
samp.size<-250
noise.level<-0.1

res.mse<-list(mds=rep(0,sim.size), duchon=rep(0,sim.size),soap=rep(0,sim.size),tprs=rep(0,sim.size))
res.edf<-list(mds=rep(0,sim.size), duchon=rep(0,sim.size),soap=rep(0,sim.size),tprs=rep(0,sim.size))

for(i in 1:sim.size){
   res<-ramsay_smooth_test(samp.size=samp.size,noise.level=noise.level,plot.it=FALSE,
                             bnd,my.grid,grid.mds2,grid.mds3,xx,yy,onoff,faster=faster)
   res.mse$mds[i]<-res$mds
   res.mse$duchon[i]<-res$duchon
   res.mse$soap[i]<-res$soap
   res.mse$tprs[i]<-res$tprs
   res.edf$mds[i]<-res$mds.edf
   res.edf$duchon[i]<-res$duchon.edf
   res.edf$soap[i]<-res$soap.edf
   res.edf$tprs[i]<-res$tprs.edf
}
write.csv(res.mse,file=paste("ramsay-mse-",samp.size,"-",noise.level,".csv",sep=""))
write.csv(res.edf,file=paste("ramsay-edf-",samp.size,"-",noise.level,".csv",sep=""))

