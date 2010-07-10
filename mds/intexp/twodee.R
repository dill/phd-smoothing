# 2d testing of the integration

library(mgcv)
library(soap)
source("mds.R")
source("inSide.R")
#source("intexp/smooth2NA.c.R")

# from the example in gam()
set.seed(0) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)

dat<-data.frame(x=dat$x0,y=dat$x1,z=dat$y)
dat1<-dat

b1<-gam(z~s(x,y),data=dat)
par(mfrow=c(2,2))
vis.gam(b1,plot.type="contour")

bnd.mds<-list(x=c(1,1,0,0,1),y=c(1,0,0,1,1))

source("intexp/smooth2s.c.R")

squash2<-function(dat,lims,sq){
   # squash the points in dat in the square lims[i,1:4],lims[i,5:8]
   # by a factor of sq[1] and sq[2] in x and y directions resp.

   # result
   res<-dat

   j<-1

   for(i in seq(1,length(lims[,1]),5) ){
      # make the boundary
      bnd<-list(x=lims[i:(i+4),1],y=lims[i:(i+4),2])
      ind<-inSide(bnd,dat$x,dat$y)
      res$x[ind]<-(res$x[ind]-min(bnd$x))/sq[j,1]+min(bnd$x)
      res$y[ind]<-(res$y[ind]-min(bnd$y))/sq[j,2]+min(bnd$y)
      #res$y[ind]<-(res$y[ind]-min(bnd$y))/sq[j,2] +min(bnd$y)
      j<-j+1
   }
   return(res)
}

bnd<-bnd.mds

box1<-t(matrix(c(0,0,0,0.5,1,0.5,1,0,0,0),2,5))
box2<-t(matrix(c(0,0.5,0,1,1,1,1,0.5,0,0.5),2,5))
lims<-rbind(box1,box2)

sq<-matrix(c(1,1,1,0.25),2,2)

#dat<-make_soap_grid(bnd,25)
res<-squash2(dat,lims,sq)

dat<-data.frame(x=dat$x,y=dat$y,z=dat1$z)
res<-data.frame(x=res$x,y=res$y,z=dat1$z)

b2<-gam(z~s(x,y),data=res)
#vis.gam(b2,plot.type="contour")

#plot(res,asp=1)

b4<-gam(z~s(x,y,bs="mdstps",xt=list(bnd.mds=bnd,
                                    sq=sq,
                                    lims=lims,
                                    bnd=bnd
#                                    op=sg,
#                                    b.grid=c(50,50),
#                                    mds.obj=grid.mds
         )),data=res)

#vis.gam(b4,plot.type="contour")

pdat<-make_soap_grid(bnd,50,mat=TRUE)
pred<-data.frame(x=pdat$x,y=pdat$y)
pres<-squash2(pred,lims,sq)

predp<-predict(b4,pres)

xx<-seq(min(bnd$x),max(bnd$x),len=49)
yy<-seq(min(bnd$y),max(bnd$y),len=49)

image(z=matrix(predp,49,49),x=xx,y=yy,main="fix")
contour(z=matrix(predp,49,49),x=xx,y=yy,add=TRUE,col="green")



pxxyy<-squash2(list(x=xx,y=yy),lims,sq)
pxxyy$y[49]<-pxxyy$y[48]+diff(pxxyy$y)[47]

predp<-predict(b2,pres)
image(z=matrix(predp,49,49),x=pxxyy$x,y=pxxyy$y,main="unfix")
contour(z=matrix(predp,49,49),x=pxxyy$x,y=pxxyy$y,add=TRUE,col="green")


image(z=matrix(predp,49,49),x=xx,y=yy,main="unfix")
contour(z=matrix(predp,49,49),x=xx,y=yy,add=TRUE,col="green")
