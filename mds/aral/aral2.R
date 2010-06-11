# load the fitted model data file and look at what happened.


load("aral/aralfit.RData")


# find zlim max and min
zlims<-c(min(c(aral$chl,tp.pred,soap.pred,mds.pred),na.rm=TRUE),
        max(c(aral$chl,tp.pred,soap.pred,mds.pred),na.rm=TRUE))


par(mfrow=c(2,2),las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)
# raw
image(z=matrix(aral$chl,46,46),x=aral.lab$km.e,y=aral.lab$km.n,
      asp=1,main="raw data",xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,zlim=zlims)
lines(bnd,lwd=2)
# tprs
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-tp.pred
image(z=pred.mat,x=unique(gxx),y=unique(gyy),main="tprs",
      xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,zlim=zlims,asp=1)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5)
lines(bnd,lwd=2)
# soap
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-soap.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),
      xlab="km (East)",ylab="km (North)",main="soap",
      xlim=xlims,ylim=ylims,asp=1,zlim=zlims)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5)
lines(bnd,lwd=2)

# mdstp
#par(las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-mds.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),
      main="mds",
xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,asp=1,zlim=zlims)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5)
lines(bnd,lwd=2)



