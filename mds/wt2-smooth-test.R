# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")

wt2_smooth_test<-function(samp.size=250,noise.level=0.05){

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


   ### create the sample
   samp.ind<-sample(1:length(gendata$x),samp.size)

   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind])

   # add noise
   noise<-noise.level*rnorm(length(samp.ind))

   # non-mapped sample data
   nsamp.data<-list(x=c(),y=c(),z=c())
   nsamp.data$x<-gendata$x[samp.ind]
   nsamp.data$y<-gendata$y[samp.ind]
   nsamp.data$z<-gendata$z[samp.ind]+noise

   ### create prediction data
   # non-mapped prediction data
   npred.data<-list(x=c(),y=c(),z=c())
   npred.data$x<-gendata$x
   npred.data$y<-gendata$y

   
   ### soap
   knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
   knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
   insideknots<-inSide(bnd,knots.x,knots.y)
   insideknots[158]<-FALSE;insideknots[56]<-FALSE;insideknots[141]<-FALSE
   knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
   b.soap<-gam(z~s(x,y,k=49,bs="so",xt=list(bnd=list(bnd))),knots=knots,data=nsamp.data)
   fv.soap <- predict(b.soap,newdata=gendata)

   ### calculate MSEs
   mse<-mean((fv.soap-gendata$z)^2,na.rm=T)

   catch<-NA
   if(mse>5){
      catch<-nsamp.data
   }

   # short object for long sims
   ret<-list(mse=mse,catch=catch)

   return(ret)

}

