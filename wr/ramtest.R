# function to do the smoothing on the Ramsay horseshoe 
# Copyright David Lawrence Miller 2009
#source("mds.R")
#source("tps.R")

source("wr-wrapper.R")

#set.seed(12)

samp.size<-250
noise.level<-0.5
n.knots<-30

## create a boundary...
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
# create points within the boundary 
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]


# make the sample
samp.ind<-sample(1:length(xx),samp.size)

# add noise
noise<-rnorm(samp.size)*noise.level

samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                      z=fs.test(xx[samp.ind],yy[samp.ind])+noise)


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
#
#
## truth
#image(xm,yn,z.truth,col=heat.colors(100),xlab="x",ylab="y",main="truth",las=1,asp=1)
#contour(xm,yn,z.truth,levels=seq(-5,5,by=.25),add=TRUE)
#lines(fsb,lwd=2)
#
#xp<-cbind(xx,yy)
#pred.mat<-matrix(NA,m,n)
##pred.mat[onoff]<-eval.tps(xp,beta,xk)
#pred.mat[onoff]<-predict(beta,xp)
#image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tps",las=1,asp=1)
#contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
#lines(fsb,lwd=2)


########################################
# now with the distances

beta<-wr(samp.data,list(x=xk[,1],y=xk[,2]),bnd)

#pred.mat<-matrix(NA,m,n)
##pred.mat[onoff]<-wr.pred(list(x=xp[,1],y=xp[,2]),list(x=xk[,1],y=xk[,2]),beta)
#pred.mat[onoff]<-predict(beta,list(x=xp[,1],y=xp[,2]))
#image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tps+dists",las=1,asp=1)
#contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
#lines(fsb,lwd=2)


### plot the parts of the smooth
#x11()
#par(mfrow=c(1,2))
#
## plot the knots' contribution
#beta1<-beta
#beta1[4:length(beta)]<-0
#pred.mat<-matrix(NA,m,n)
#pred.mat[onoff]<-predict(beta1,list(x=xp[,1],y=xp[,2]))
#image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="knots",las=1,asp=1)
#contour(xm,yn,pred.mat,levels=seq(min(pred.mat,na.rm=TRUE),max(pred.mat,na.rm=TRUE),by=2),add=TRUE)
#lines(fsb,lwd=2)
#
## plot the nullspace
#beta2<-beta
#beta2[4:length(beta)]<-0
#pred.mat<-matrix(NA,m,n)
#pred.mat[onoff]<-rep(beta[n.knots],nrow(xp))+beta[n.knots+2]*xp[,1] + beta[n.knots+3]*xp[,2]
#image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="nullspace",las=1,asp=1)
#contour(xm,yn,pred.mat,levels=seq(min(pred.mat,na.rm=TRUE),max(pred.mat,na.rm=TRUE),by=0.25),add=TRUE)
#lines(fsb,lwd=2)
#
#
#
## each knot...
#knots<-attr(beta,"knots")
#D.xpxk<-create_distance_matrix(
#            c(xp[,1],knots$x),
#            c(xp[,2],knots$y),bnd,
#            start=length(xp[,1]))
#
#
#preds<-Predict.matrix.tps(beta,xp,D.xpxk)
#
##par(mfrow=c(5,6))
#
#for(i in 1:33){
#   pred.mat[onoff]<-preds[,i]
#   image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="nullspace",las=1,asp=1)
#   contour(xm,yn,pred.mat,levels=seq(min(pred.mat,na.rm=TRUE),
#                                     max(pred.mat,na.rm=TRUE),by=0.25),add=TRUE)
#   if(i<30){points(xk[i,1],xk[i,2],pch=19)}
#scan()
#}



