# load the fitted model data file and look at what happened.

library(soap)
load("aral/aralfit.RData")

#pdf(file="../thesis/mds/figs/aral-pp.pdf",width=5,height=2)
#par(mfrow=c(1,2),las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

grid.mds<-cmdscale(D,eig=TRUE,k=3,x.ret=TRUE)

# create the data frame and fit the model
aral.mds<-insert.mds(aral.dat,mds.grid,grid.mds,bnd,faster=1)
aral.mds<-data.frame(x=aral.mds[,1],
                     y=aral.mds[,2],
                     z=aral.mds[,3],
                     chl=aral.dat$chl)

# fit the model
mds.fit<-gam(chl~s(x,y,z,k=140),data=aral.mds,family=Gamma(link="log"))

# mds prediction grid
pred.grid.mds<-insert.mds(pred.grid,mds.grid,grid.mds,bnd,faster=1)
pred.grid.mds<-data.frame(x=pred.grid.mds[,1],
                          y=pred.grid.mds[,2],
                          z=pred.grid.mds[,3])

# do the prediction
mds.pred<-predict(mds.fit,newdata=pred.grid.mds,type="response")

zlims<-c(1.905461, 19.275249)


pdf(file="../thesis/mds/figs/aral-3d.pdf",width=2.5,height=3)
par(las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)


pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-mds.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),
      xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,asp=1,zlim=zlims)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)



dev.off()
