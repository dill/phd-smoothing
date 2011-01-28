# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
 
library(mdspack)

set.seed(123)
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

samp.size<-250
noise.level<-0.9


while(1){

   this.seed<-get(".Random.seed",envir=.GlobalEnv) ## store RNG seed

   ## create the sample
   samp.ind<-sample(1:length(gendata$x),samp.size)
   noise<-noise.level*rnorm(length(samp.ind))
   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind]+noise)

 
   # fit with tp

   # fit with Duchon 



   # create the image
   gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
   ind<-c(1:length(gendata.ind$x))
   pred.mat<-rep(NA,length(gendata.ind$x))
   ind<-ind[gendata.ind$inside==1]
   na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
   ind<-ind[na.ind]
   ind<-ind[onoff]
 
 
      # plot for truth, mds, tprs and soap
      par(mfrow=c(1,3),mar=c(1.8,1.5,1.8,1.5),las=1)
      

      # axis scales
      xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
      yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)
   
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-gendata.ind$z[ind]
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="Truth",asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
      contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)
   
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.s
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="mds+tprs",asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
      contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)
      

      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.tprs
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="tprs",asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
      contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)
      
 
   ### calculate MSEs
   mses<-list(mds=mean((fv.s-gendata.ind$z[ind])^2,na.rm=T),
#              mdstp=mean((fv.te-gendata.ind$z[ind])^2,na.rm=T),
              tprs=mean((fv.tprs-gendata.ind$z[ind])^2,na.rm=T),
              soap=mean((fv.soap-gendata.ind$z[ind])^2,na.rm=T))
 
   # print them
   #cat("mds MSE=" ,mses$mds,"\n")
   #cat("tprs MSE=",mses$tprs,"\n")
   #cat("soap MSE=",mses$soap,"\n")
 
}
