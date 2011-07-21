# smooth.* testing

library(mgcv)
library(soap)
library(mdspack)


source("mdspack/R/smooth.construct.msg.smooth.spec.R")
source("mdspack/R/extrapenalty.R")

# horseshoe

bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
# create points within the boundary 
#m<-45;n<-25
m<-90;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
hs<-data.frame(x=xx,y=yy,z=fs.test(xx,yy))
rm(xx); rm(yy); gc()

set.seed(123)

dat<-hs[sample(1:length(hs$x),250),]
dat$z<-dat$z+rnorm(250)*0.05



### fit without extra penalty
b<-gam(z~s(x,y,bs="msg",xt=list(bnd=bnd,mds.dim=3,extra.penalty=FALSE)),data=dat)

pp<-predict(b,data.frame(x=hs$x,y=hs$y))
zmat<-matrix(NA,m,n)
zmat[onoff]<-pp


# fit with extra penalty
b.ep<-gam(z~s(x,y,bs="msg",xt=list(bnd=bnd,mds.dim=3,extra.penalty=TRUE)),data=dat)

pp.ep<-predict(b.ep,data.frame(x=hs$x,y=hs$y))
zmat.ep<-matrix(NA,m,n)
zmat.ep[onoff]<-pp.ep


par(mfrow=c(1,2))

image(z=zmat,x=xm,y=yn,main="without extra.penalty",asp=1,col=heat.colors(1000))
contour(z=zmat,x=xm,y=yn,main="without extra.penalty",add=TRUE)
image(z=zmat.ep,x=xm,y=yn,main="with extra.penalty",asp=1,col=heat.colors(1000))
contour(z=zmat.ep,x=xm,y=yn,main="without extra.penalty",add=TRUE)

