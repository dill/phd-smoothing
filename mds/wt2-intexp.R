# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")

set.seed(1)
 
samp.size=250
noise.level=0.05

library(ks)
 
## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
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



gendata<-list(x=gendata$x[-samp.ind],
               y=gendata$y[-samp.ind],
               z=gendata$z[-samp.ind])

# create the grid
my.grid<-create_refgrid(bnd,120)

## do the MDS on the grid 
# create D
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)

# perform mds on D
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)

# sample points insertion
samp.mds<-insert.mds(gendata.samp,my.grid,grid.mds,bnd,faster=1)

# prediction points insertion
pred.mds<-insert.mds(gendata,my.grid,grid.mds,bnd,faster=1)

grid.mds<-grid.mds$points

# add noise
noise<-noise.level*rnorm(length(samp.ind))
#> summary(gendata$z)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
#0.000000 0.000236 0.269300 0.276300 0.479600 0.850000

# mapped sample data
samp.data<-list(x=c(),y=c(),z=c())
samp.data$x<-samp.mds[,1]
samp.data$y<-samp.mds[,2]
samp.data$z<-gendata.samp$z+noise

# non-mapped sample data
nsamp.data<-list(x=c(),y=c(),z=c())
nsamp.data$x<-gendata.samp$x
nsamp.data$y<-gendata.samp$y
nsamp.data$z<-gendata.samp$z+noise

### create prediction data
# non-mapped prediction data
npred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                y=rep(0,length(gendata$x)+length(samp.data$y)))
npred.data$x[-samp.ind]<-gendata$x
npred.data$y[-samp.ind]<-gendata$y
npred.data$x[samp.ind]<-nsamp.data$x
npred.data$y[samp.ind]<-nsamp.data$y


# put this in the correct format
pred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                y=rep(0,length(gendata$x)+length(samp.data$y)))
pred.data$x[-samp.ind]<-pred.mds[,1]
pred.data$y[-samp.ind]<-pred.mds[,2]
pred.data$x[samp.ind]<-samp.mds[,1]
pred.data$y[samp.ind]<-samp.mds[,2]
 
#   ind<-pred.data$x<min(samp.data$x)
#   pred.data$x[ind]<-NA
#   pred.data$y[ind]<-NA
#
#   ind<-pred.data$x>max(samp.data$x)
#   pred.data$x[ind]<-NA
#   pred.data$y[ind]<-NA
#
#   ind<-pred.data$y<min(samp.data$y)
#   pred.data$x[ind]<-NA
#   pred.data$y[ind]<-NA
#
#   ind<-pred.data$y>max(samp.data$y)
#   pred.data$x[ind]<-NA
#   pred.data$y[ind]<-NA

   ### Now do some fitting and prediction
   ### pspline 
   b.ps<-gam(z~s(x,y,k=80),data=samp.data)
   fv.ps<-predict(b.ps,newdata=pred.data)

source("intexp/smooth2.c.R")

   # clever psline 
   b.mdsps<-gam(z~s(x,y,k=80,bs="mdstp"),data=samp.data)
   fv.mdsps <- predict(b.mdsps,newdata=pred.data)

   # create the image
   gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
   ind<-c(1:length(gendata.ind$x))
   pred.mat<-rep(NA,length(gendata.ind$x))
   ind<-ind[gendata.ind$inside==1]
   na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
   ind<-ind[na.ind]
   ind<-ind[onoff]
 
 
      # plot for truth, mds, tprs and soap
      par(mfrow=c(2,2))
      par(mar=c(3,3,3,3))
      
      # axis scales
      xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
      yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)
   
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-gendata.ind$z[ind]
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="True",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
      contour(xscale,yscale,pred.mat,add=T)
   
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.ps
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="MDS + ps",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
      contour(xscale,yscale,pred.mat,add=T)
      
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.mdsps
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="MDS + ps + mod",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
      contour(xscale,yscale,pred.mat,add=T)

 
   ### calculate MSEs
   mses<-list(ps=mean((fv.ps-gendata.ind$z[ind])^2,na.rm=T),
              mdsps=mean((fv.mdsps-gendata.ind$z[ind])^2,na.rm=T))

cat("old=",mses$ps,"\n")
cat("new=",mses$mdsps,"\n")
