# attempt to duplicate the simulation settings in 
# Wang and Ranalli (2007)
# Copyright David Lawrence Miller 2010
#source("mds.R")
#source("tps.R")

source("wr-wrapper.R")

# import fields for the cover.design() function
library(fields)


#set.seed(12)

# as in the paper
samp.size<-100
noise.level<-0.05
n.knots<-40
replicates<-200


## create a boundary...
bnd <- fs.boundary()
#bnd<-pe(bnd,seq(1,length(bnd$x),6))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
# create points within the boundary 
#m<-50;n<-30 # with every 6th point kept
m<-45;n<-30 # 950 points with full bnd  
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]


# knot selection



# pre-calculate the distances





for(i in 1:replicates){

   # make the sample
   samp.ind<-sample(1:length(xx),samp.size)
   
   # add noise
   noise<-rnorm(samp.size)*noise.level
   
   samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                         z=fs.test(xx[samp.ind],yy[samp.ind])+noise)
   


   # fit with tprs



   # fit with mds



   # fit with wr


   
   #######################################
   # now try some fitting...
   
   # put things in a nice format for fit.tps
   y<-samp.data$z
   x<-cbind(samp.data$x,samp.data$y)
   
   # first find some knots (at random)
   nn<-length(y)
   ind <- sample(1:nn,n.knots,replace=FALSE)
   xk <- x[ind,]
   
   # do the fitting
   beta <- fit.tps(y,x,xk=xk)
   
   #######################################
   # ploting...
   
   # boundary, only for drawing the line around the outside
   fsb <- fs.boundary()
   
   # truth
   z.truth<-matrix(NA,m,n)
   z.truth[onoff]<-fs.test(xx,yy)
   
   #par(mfrow=c(1,3))
   
   
   # truth
   #image(xm,yn,z.truth,col=heat.colors(100),xlab="x",ylab="y",main="truth",las=1,asp=1)
   #contour(xm,yn,z.truth,levels=seq(-5,5,by=.25),add=TRUE)
   #lines(fsb,lwd=2)
   
   #xp<-cbind(xx,yy)
   #pred.mat<-matrix(NA,m,n)
   #pred.mat[onoff]<-eval.tps(xp,beta,xk)
   #image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tps",las=1,asp=1)
   #contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
   #lines(fsb,lwd=2)
   
   
   ########################################
   # now with the distances
   
   beta<-wr(samp.data,list(x=xk[,1],y=xk[,2]),bnd)
   
   #pred.mat<-matrix(NA,m,n)
   #pred.mat[onoff]<-wr.pred(list(x=xp[,1],y=xp[,2]),list(x=xk[,1],y=xk[,2]),beta)
   #image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tps+dists",las=1,asp=1)
   #contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
   #lines(fsb,lwd=2)


   # calculate MSE


   # calculate MSSE


   # calculate bias


   # calculate artifactiness


}
