# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("wr-wrapper.R")
 
samp.size=250
noise.level=0.9
 
## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")
gendata<-read.csv("wt2truth.csv",header=TRUE)
gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])
na.ind<-!(is.na(gendata$x)&is.na(gendata$y)&is.na(gendata$z))
gendata<-list(x=gendata$x[na.ind],
               y=gendata$y[na.ind],
               z=gendata$z[na.ind])
# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
onoff<-inSide(bnd.neg,-gendata$x,-gendata$y)
gendata<-list(x=gendata$x[onoff],
               y=gendata$y[onoff],
               z=gendata$z[onoff])
# create the sample index
samp.ind<-sample(1:length(gendata$x),samp.size)

## create the sample
gendata.samp<- list(x=gendata$x[samp.ind],
                    y=gendata$y[samp.ind],
                    z=gendata$z[samp.ind])


# add noise
noise<-noise.level*rnorm(length(samp.ind))
gendata.samp$z<-gendata.samp$z+noise

# put this in the correct format
pred.data<-gendata


# truth 
gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
ind<-c(1:length(gendata.ind$x))
pred.mat<-rep(NA,length(gendata.ind$x))
ind<-ind[gendata.ind$inside==1]
na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
ind<-ind[na.ind]
ind<-ind[onoff]
plot.ind<-ind

# plot for truth, mds, tprs and soap
par(mfrow=c(3,1),mar=c(1.8,1.5,1.8,1.5),las=1)

# axis scales
xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)

pred.mat<-rep(NA,length(gendata.ind$x))
pred.mat[ind]<-gendata.ind$z[ind]
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="Truth",asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)

# normal tprs
b.tprs<-gam(z~s(x,y,k=100),data=gendata.samp)
fv.tprs <- predict(b.tprs,newdata=pred.data)

# plot tprs
pred.mat<-rep(NA,length(gendata.ind$x))
pred.mat[ind]<-fv.tprs
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="tprs",asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)


###########################################################################
# W+R

# first get some knots
n.knots<-50
nn<-length(gendata.samp$y)
ind <- sample(1:nn,n.knots,replace=FALSE)
xk<-pe(gendata.samp,ind)

beta<-wr(gendata.samp,xk,bnd,lambda=b.tprs$sp)

pred.mat<-matrix(NA,length(xscale),length(yscale))
pred.mat[plot.ind]<-wr.pred(pred.data,xk,beta)
image(xscale,yscale,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tps+dists",las=1,asp=1)
contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)

