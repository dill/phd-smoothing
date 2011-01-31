# does the artefact disappear?
# plotting code...

library(dillhandy)
library(mdspack)

plot.pred<-function(mod.obj,title=""){
   predtry<-gam.mds(gendata.samp,gendata,bnd,old.obj=mod.obj)
   gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
   ind<-c(1:length(gendata.ind$x))
   pred.mat<-rep(NA,length(gendata.ind$x))
   ind<-ind[gendata.ind$inside==1]
   na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
   ind<-ind[na.ind]
   ind<-ind[onoff]
   
   xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
   yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)
   
   pred.mat<-rep(NA,length(gendata.ind$x))
   pred.mat[ind]<-predtry$pred
   pred.mat<-matrix(pred.mat,50,50)
   image(xscale,yscale,pred.mat,main=title,asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
   contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)
}


# load the analysis that produced it...
load("artefact-maybe.RData")


# the original (MDS 2D, no Duchon) model is in a


# create some plot windows
par(mfrow=c(3,2),mar=c(1.8,1.5,1.8,1.5),las=1)


std.mds<-a
plot.pred(std.mds,"2D MDS")


par(mfrow=c(4,3),mar=c(1.8,1.5,1.8,1.5),las=1)
for(mds.dim in c(2,3,4,5)){
   for(d.val in c(2,3,4)){
      
      mds.mod<-gam.mds(gendata.samp,NULL,bnd,mds.dim=mds.dim,bs="ds",m=c(d.val,d.val/2-1))
      plot.pred(mds.mod,paste(mds.dim,"D MDS d=",d.val,sep=""))
   }
}


#### try some other models

# 2D MDS + Duchon
mds.2d.d<-gam.mds(gendata.samp,NULL,bnd,mds.dim=2,bs="ds",m=c(2,0.5))
plot.pred(mds.2d.d,"2D MDS (Duchon)")

# 3D MDS
mds.3d<-gam.mds(gendata.samp,NULL,bnd,mds.dim=3)
plot.pred(mds.3d,"3D MDS")

# 3D MDS + Duchon
mds.3d.d<-gam.mds(gendata.samp,NULL,bnd,mds.dim=3,bs="ds",m=c(2,3/2-1))
plot.pred(mds.3d.d,"3D MDS (Duchon)")

### More?

# 4D MDS
mds.4d<-gam.mds(gendata.samp,NULL,bnd,mds.dim=4)
plot.pred(mds.4d,"4D MDS")

# 4D MDS + Duchon
mds.4d.d<-gam.mds(gendata.samp,NULL,bnd,mds.dim=4,bs="ds",m=c(2,4/2-1))
plot.pred(mds.4d.d,"4D MDS (Duchon)")


## 5D MDS + Duchon
#mds.4d.d<-gam.mds(gendata.samp,NULL,bnd,mds.dim=5,bs="ds",m=c(5,5/2-1))
#plot.pred(mds.4d.d,"5D MDS (Duchon)")

