# signal to noise ratio...
library(soap)
samp.size<-250
noise.level<-1.55


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

corr<-c()
for(i in 1:200){

   # create the sample index
   samp.ind<-sample(1:length(gendata$x),samp.size)

   ## create the sample
   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind])

   noise<-noise.level*rnorm(length(samp.ind))

   nsamp.data<-list(x=c(),y=c(),z=c())
   nsamp.data$x<-gendata.samp$x
   nsamp.data$y<-gendata.samp$y
   nsamp.data$z<-gendata.samp$z+noise

   corr<-c(corr,cor(gendata.samp$z,nsamp.data$z)^2)
}
cat(mean(corr),"\n")
