# now in...
#          ____  _____  
#         |___ \|  __ \ 
#           __) | |  | |
#          |__ <| |  | |
#          ___) | |__| |
#         |____/|_____/ 
#              
              


# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")

wt2_smooth_test_3d<-function(samp.size=250,noise.level=0.05, plot.it=FALSE){

   ## create a boundary...
   bnd <- read.csv("wt2-verts.csv",header=FALSE) 
   names(bnd)<-c("x","y")
   
   ## Simulate some fitting data, inside boundary...
   gendata <- read.csv("wt2truth.csv",header=TRUE) 
   
   gendata<- list(x=gendata$x[gendata$inside==1],
                  y=gendata$y[gendata$inside==1],
                  z=gendata$z[gendata$inside==1])
   
   na.ind<-!(is.na(gendata$x)&is.na(gendata$y)&is.na(gendata$z))
   
   gendata<- list(x=gendata$x[na.ind],
                  y=gendata$y[na.ind],
                  z=gendata$z[na.ind])
   
   # attempt to get around the inside bug
   bnd.neg<-list(x=-bnd$x,y=-bnd$y)
   onoff<-inSide(bnd.neg,-gendata$x,-gendata$y)
   
   gendata<- list(x=gendata$x[onoff],
                  y=gendata$y[onoff],
                  z=gendata$z[onoff])

   # create the sample
   samp.ind<-sample(1:length(gendata$x),samp.size)

   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind])

   ### do the PCO and construct the data frame
   # create D
   D<-create_distance_matrix(gendata.samp$x,gendata.samp$y,bnd)

   # perform mds on the sample matrix
   # options needed for insertion to work
   samp.mds<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=3)
   
   # add noise
   noise<-noise.level*rnorm(length(samp.ind))
   #> summary(gendata$z)
   #    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
   #0.000000 0.000236 0.269300 0.276300 0.479600 0.850000 

   # mapped sample data
   samp.data<-list(x=c(),y=c(),z=c(),w=c())
   samp.data$x<-samp.mds$points[,1]
   samp.data$y<-samp.mds$points[,2]
   samp.data$w<-samp.mds$points[,3]
   samp.data$z<-gendata$z[samp.ind]+noise

   # non-mapped sample data
   nsamp.data<-list(x=c(),y=c(),z=c())
   nsamp.data$x<-gendata$x[samp.ind]
   nsamp.data$y<-gendata$y[samp.ind]
   nsamp.data$z<-gendata$z[samp.ind]+noise

   ### create prediction data
   # non-mapped prediction data
   npred.data<-list(x=c(),y=c(),z=c())
   npred.data$x<-gendata$x[-samp.ind]
   npred.data$y<-gendata$y[-samp.ind]

   # new MDS coords for the prediction points
   pred.mds<-insert.mds(npred.data,gendata.samp,samp.mds,bnd)

   # put this in the correct format 
   pred.data<-list(x=rep(0,length(gendata$x)),y=rep(0,length(gendata$x)),w=rep(0,length(gendata$x)))
   pred.data$x[samp.ind]<-samp.data$x  # need to add in the sample points too
   pred.data$x[-samp.ind]<-pred.mds[,1]
   pred.data$y[samp.ind]<-samp.data$y  # need to add in the sample points too
   pred.data$y[-samp.ind]<-pred.mds[,2]
   pred.data$w[samp.ind]<-samp.data$w  # need to add in the sample points too
   pred.data$w[-samp.ind]<-pred.mds[,3]

   ### Now do some fitting and prediction
   ### mapping
   b.mapped<-gam(z~s(x,y,w,k=49),data=samp.data)
   fv <- predict(b.mapped,newdata=pred.data)
   
   ### normal tprs
   b.tprs<-gam(z~s(x,y,k=49),data=nsamp.data)
   fv.tprs <- predict(b.tprs,newdata=gendata)
   
   ### soap
   knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
   knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
   insideknots<-inSide(bnd,knots.x,knots.y)
   insideknots[158]<-FALSE;insideknots[56]<-FALSE;insideknots[141]<-FALSE
   knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
   b.soap<-gam(z~s(x,y,k=49,bs="so",xt=list(bnd=list(bnd))),knots=knots,data=nsamp.data)
   fv.soap <- predict(b.soap,newdata=gendata)

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
      par(mfrow=c(2,2))
      
      # axis scales
      xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
      yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)
   
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-gendata$z
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="truth",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
      contour(xscale,yscale,pred.mat,add=T)
      
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="mds",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
      contour(xscale,yscale,pred.mat,add=T)
      
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.tprs
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="tprs",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
      contour(xscale,yscale,pred.mat,add=T)
      
      pred.mat<-rep(NA,length(gendata.ind$x))
      pred.mat[ind]<-fv.soap
      pred.mat<-matrix(pred.mat,50,50)
      image(xscale,yscale,pred.mat,main="soap",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
      contour(xscale,yscale,pred.mat,add=T)

   }


   
   ### calculate MSEs
   mses<-list(mds=mean((fv-gendata$z)^2,na.rm=T),
              tprs=mean((fv.tprs-gendata$z)^2,na.rm=T),
              soap=mean((fv.soap-gendata$z)^2,na.rm=T))

   # print them
   #cat("mds MSE=" ,mses$mds,"\n")
   #cat("tprs MSE=",mses$tprs,"\n")
   #cat("soap MSE=",mses$soap,"\n")

   # lets return an object...

#   ret<-list(samp.mds=samp.data,pred.mds=pred.data,samp=nsamp.data,pred=npred.data,mses=mses)

   # short object for long sims
   ret<-list(mses=mses)

   return(ret)

}

#### MDS explanatory plot
#par(mfrow=c(1,2))
#red.points<-data.mapped$x < -2
#green.points<-data.mapped$y>1 & data.mapped$x<0
#blue.points<-data.mapped$y>-0.5 & data.mapped$x>2
#orange.points<-gendata$y>-1 & gendata$x>-0.5 & gendata$x<1
#
#plot(gendata$x,gendata$y,asp=1,las=1,xlab="x",ylab="y",cex=0.5,pch=20)
#points(gendata$x[red.points],gendata$y[red.points],cex=0.5,pch=20,col="red")
#points(gendata$x[green.points],gendata$y[green.points],cex=0.5,pch=20,col="green")
#points(gendata$x[blue.points],gendata$y[blue.points],cex=0.5,pch=20,col="blue")
#points(gendata$x[orange.points],gendata$y[orange.points],cex=0.5,pch=20,col="orange")
#
#plot(data.mapped$x,data.mapped$y,asp=1,las=1,xlab="x",ylab="y",cex=0.5,pch=20)
#points(data.mapped$x[red.points],data.mapped$y[red.points],cex=0.5,pch=20,col="red")
#points(data.mapped$x[green.points],data.mapped$y[green.points],cex=0.5,pch=20,col="green")
#points(data.mapped$x[blue.points],data.mapped$y[blue.points],cex=0.5,pch=20,col="blue")
#points(data.mapped$x[orange.points],data.mapped$y[orange.points],cex=0.5,pch=20,col="orange")
#
#
#dev.copy2pdf(file="wt2-mds-coloured.pdf")


