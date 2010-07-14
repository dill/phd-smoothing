# do large scale simulations for wt2
# Copyright David Lawrence Miller 2009.


source("mds.R")
source("ramsay-smooth-test-time.R")

faster<-0

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

# make a grid
my.grid<-make_soap_grid(bnd,12)
grid.time<-system.time(D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=faster))[3]
grid.time<-grid.time+system.time(grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE))[3]




#################################

sim.size<-100
samp.size<-250
noise.level<-1

res.gam<-list(mds=rep(0,sim.size), soap=rep(0,sim.size),tprs=rep(0,sim.size))
res.pred<-list(mds=rep(0,sim.size), soap=rep(0,sim.size),tprs=rep(0,sim.size))

for(i in 1:sim.size){
   res<-ramsay_smooth_test(samp.size=samp.size,noise.level=noise.level,plot.it=FALSE,
                             bnd,my.grid,grid.mds,xx,yy, onoff,faster=faster)
   res.gam$mds[i]<- res$mds[1]+grid.time
   res.gam$soap[i]<-res$soap[1]
   res.gam$tprs[i]<-res$tprs[1]

   res.pred$mds[i]<- res$mds[2]+grid.time
   res.pred$soap[i]<-res$soap[2]
   res.pred$tprs[i]<-res$tprs[2]
}
#write.csv(res.mse,file=paste("ramsay-",samp.size,"-",noise.level,".csv",sep=""))

sink(file="ramsay-times.txt")
cat(mean(res.gam$mds),"\n")
cat(mean(res.gam$soap),"\n")
cat(mean(res.gam$tprs),"\n")
cat(mean(res.pred$mds),"\n")
cat(mean(res.pred$soap),"\n")
cat(mean(res.pred$tprs),"\n")
sink()
