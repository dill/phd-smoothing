# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
 
library(mdspack)

#this.seed<-get(".Random.seed",envir=.GlobalEnv) ## store RNG seed

wt2_smooth_test<-function(samp.size=250,noise.level=0.9,plot.it=FALSE){
 
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

   # add noise
   noise<-noise.level*rnorm(length(samp.ind))
   #> summary(gendata$z)
   # Min. 1st Qu. Median Mean 3rd Qu. Max.
   #0.000000 0.000236 0.269300 0.276300 0.479600 0.850000
 
   # non-mapped sample data
   nsamp.data<-list(x=c(),y=c(),z=c())
   nsamp.data$x<-gendata.samp$x
   nsamp.data$y<-gendata.samp$y
   nsamp.data$z<-gendata.samp$z+noise
 
   ### create prediction data
   # non-mapped prediction data
   npred.data<-list(x=rep(0,length(gendata$x)+length(nsamp.data$x)),
                   y=rep(0,length(gendata$x)+length(nsamp.data$y)))
   npred.data$x[-samp.ind]<-gendata$x
   npred.data$y[-samp.ind]<-gendata$y
   npred.data$x[samp.ind]<-nsamp.data$x
   npred.data$y[samp.ind]<-nsamp.data$y
 
   ### Now do some fitting and prediction
   # tensor thin plate   
   #b.mapped.te<-gam(z~te(x,y,k=12),data=samp.data)
   #fv.te <- predict(b.mapped.te,newdata=pred.data)

   # mapping
   #b.mapped.s<-gam(z~s(x,y,w,k=100,m=c(3,3/2-1),bs="ds"),data=samp.data)
   #fv.s <- predict(b.mapped.s,newdata=pred.data)
   b.mds<-gam.mds(nsamp.data,npred.data,bnd,grid.res=c(120))
   fv.s<-b.mds$pred
    
   # normal tprs
   b.tprs<-gam(z~s(x,y,k=100),data=nsamp.data)
   fv.tprs <- predict(b.tprs,newdata=npred.data)

   # soap
   knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
   knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
   insideknots<-inSide(bnd,knots.x,knots.y)
   insideknots[56]<-FALSE;insideknots[115]<-FALSE;insideknots[158]<-FALSE
   knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
   b.soap<-gam(z~s(x,y,k=60,bs="so",xt=list(bnd=list(bnd))),knots=knots,data=nsamp.data)
   fv.soap <- predict(b.soap,newdata=npred.data)
 
   # create the image
   gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
   ind<-c(1:length(gendata.ind$x))
   pred.mat<-rep(NA,length(gendata.ind$x))
   ind<-ind[gendata.ind$inside==1]
   na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
   ind<-ind[na.ind]
   ind<-ind[onoff]
 
   ## plotting
   if(plot.it){
 
      # plot for truth, mds, tprs and soap
      par(mfrow=c(2,2),mar=c(1.8,1.5,1.8,1.5),las=1)
      
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
      image(xscale,yscale,pred.mat,
            main=paste("mds+Duchon ",b.mds$mds.dim,"D",sep=""),
            asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
      contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)
      
 #     pred.mat<-rep(NA,length(gendata.ind$x))
 #     pred.mat[ind]<-fv.te
 #     pred.mat<-matrix(pred.mat,50,50)
 #     image(xscale,yscale,pred.mat,main="MDS + tptp",asp=1,xlab="",ylab="",col=heat.colors(100))
 #     contour(xscale,yscale,pred.mat,add=T)

      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.tprs
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="tprs",asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
      contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)
      
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.soap
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="soap",asp=1,xlab="",ylab="",col=heat.colors(100),cex.axis=0.5)
      contour(xscale,yscale,pred.mat,add=T,labcex=0.3,lwd=0.5)
 
   }
 
   ### calculate MSEs
   mses<-list(mds=mean((fv.s-gendata.ind$z[ind])^2,na.rm=T),
#              mdstp=mean((fv.te-gendata.ind$z[ind])^2,na.rm=T),
              tprs=mean((fv.tprs-gendata.ind$z[ind])^2,na.rm=T),
              soap=mean((fv.soap-gendata.ind$z[ind])^2,na.rm=T))
 
   # print them
   #cat("mds MSE=" ,mses$mds,"\n")
   #cat("tprs MSE=",mses$tprs,"\n")
   #cat("soap MSE=",mses$soap,"\n")
 
   # lets return an object...
   # ret<-list(samp.mds=samp.data,pred.mds=pred.data,
   #           samp=nsamp.data,pred=npred.data,mses=mses)
 
   # short object for long sims
   ret<-list(mses=mses)
 
   return(ret)
 
}
