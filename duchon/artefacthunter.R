# look for artefacts using a modified wt2...

# plot that
plot.it<-function(obj){
   pred.mat<-rep(NA,length(gendata$x))
   pred.mat[onoff]<-obj$pred
   pred.mat<-matrix(pred.mat,50,50)
   image(xscale,yscale,pred.mat,
            main=paste("mds bs=",obj$bs," s=",obj$m[2]," mds=",obj$mds.dim,sep=""),
            asp=1,xlab="",ylab="",zlim=zlims,
            col=heat.colors(100),cex.axis=0.5)
   contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5,zlim=zlims)
}
 
library(mdspack)

#set.seed(123)
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata<-read.csv("wt2truth-new.csv",header=TRUE)
onoff<-gendata$inside==1

gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

zlims<-c(min(gendata$z),max(gendata$z))

# plot for truth
par(mfrow=c(1,5),mar=c(1.8,1.5,1.8,1.5),las=1)
# axis scales
xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)
pred.mat<-rep(NA,length(gendata$x))
pred.mat[onoff]<-gendata$z
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="Truth",asp=1,xlab="",ylab="",
         col=heat.colors(100),cex.axis=0.5,zlim=zlims)
contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5,zlim=zlims)

# make a sample
samp.size<-500
noise.level<-0.9
samp.ind<-sample(1:length(gendata$x),samp.size)
noise<-noise.level*rnorm(length(samp.ind))
gendata.samp<- list(x=gendata$x[samp.ind],
                    y=gendata$y[samp.ind],
                    z=gendata$z[samp.ind]+noise)


# fit with MDS 2D and no Duchon
base.fit<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=2)
plot.it(base.fit)


base.fit$mds.dim<-NULL
base.fit$m<-NULL
base.fit$bs<-NULL
base.fit$k<-NULL


expl<-0.85

# fit with MDS choose-D WITHOUT Duchon
mse.cd<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,old.obj=base.fit)
plot.it(mse.cd)
      
# fit with MDS choose-D WITH Duchon
mse.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,old.obj=base.fit,bs="ds")
plot.it(mse.cd.ds)
 

expl<-0.9

# fit with MDS choose-D WITHOUT Duchon
#mse.cd<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,old.obj=base.fit)
#plot.it(mse.cd)
      
# fit with MDS choose-D WITH Duchon
mse.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,old.obj=base.fit,bs="ds")
plot.it(mse.cd.ds)



#   ### calculate MSEs
#   mses<-list(mds=mean((fv.s-gendata.ind$z[ind])^2,na.rm=T),
##              mdstp=mean((fv.te-gendata.ind$z[ind])^2,na.rm=T),
#              tprs=mean((fv.tprs-gendata.ind$z[ind])^2,na.rm=T),
#              soap=mean((fv.soap-gendata.ind$z[ind])^2,na.rm=T))
 
   # print them
   #cat("mds MSE=" ,mses$mds,"\n")
   #cat("tprs MSE=",mses$tprs,"\n")
   #cat("soap MSE=",mses$soap,"\n")
 
#}
