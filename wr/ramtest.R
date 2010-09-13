# function to do the smoothing on the Ramsay horseshoe 
# Copyright David Lawrence Miller 2009
source("mds.R")
source("tps.R")


set.seed(12)

samp.size<-50
noise.level<-0.05
n.knots<-10


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
beta <- fit.tps(y,x,xk=xk,lambda=.01)




#######################################
# ploting...

# boundary, only for drawing the line around the outside
fsb <- fs.boundary()

# truth
z.truth<-matrix(NA,m,n)
z.truth[onoff]<-fs.test(xx,yy)

par(mfrow=c(1,3))


# truth
image(xm,yn,z.truth,col=heat.colors(100),xlab="x",ylab="y",main="truth",las=1,asp=1)
contour(xm,yn,z.truth,levels=seq(-5,5,by=.25),add=TRUE)
lines(fsb,lwd=2)

xp<-cbind(xx,yy)
pred.mat<-matrix(NA,m,n)
pred.mat[onoff]<-eval.tps(xp,beta,xk)
image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tps",las=1,asp=1)
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
lines(fsb,lwd=2)


########################################
# now with the distances

# need to find the distance matrix
D<-create_distance_matrix(c(x[,1],xk[,1]),
                          c(x[,2],xk[,2]),bnd)
# distances from data to knots
D.xxk<-D[1:length(x[,1]),(length(x[,1])+1):dim(D)[2]]
# distances between knots
D.xkxk<-D[(length(x[,1])+1):dim(D)[2],(length(x[,1])+1):dim(D)[2]]

# for the prediction points
D.p<-create_distance_matrix(c(xp[,1],xk[,1]),
                          c(xp[,2],xk[,2]),bnd)
# distances from prediction points to knots
D.xpxk<-D.p[1:length(xp[,1]),(length(xp[,1])+1):dim(D.p)[2]]

beta <- fit.tps(y,x,xk=xk,lambda=.1,D.xkxk=D.xkxk,D.xxk=D.xxk)
pred.mat<-matrix(NA,m,n)
pred.mat[onoff]<-eval.tps(xp,beta,xk,D.xpxk=D.xpxk)
image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tps+dists",las=1,asp=1)
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
lines(fsb,lwd=2)







#### create prediction data
## non-mapped prediction data
#pred.data<-data.frame(x=xx[-samp.ind],y=yy[-samp.ind],
#                      z=fs.test(xx[-samp.ind],yy[-samp.ind]))
#
## new MDS coords for the prediction points
#pred.mds<-insert.mds(pred.data,my.grid,grid.mds,bnd)
#
## put this in the correct format 
#pred.size<-dim(pred.data)[1]+dim(samp.data)[1]
#pred.data.mds<-list(x=rep(0,pred.size),y=rep(0,pred.size))
#pred.data.mds$x[samp.ind]<-samp.data.mds$x  # need to add in the sample points too
#pred.data.mds$x[-samp.ind]<-pred.mds[,1]
#pred.data.mds$y[samp.ind]<-samp.data.mds$y  # need to add in the sample points too
#pred.data.mds$y[-samp.ind]<-pred.mds[,2]
#
## prediction data for non mds'd
#pred.data<-list(x=xx,y=yy,z=fs.test(xx,yy))
#
#

