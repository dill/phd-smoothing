# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")


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



D.full<-create_distance_matrix(gendata$x,gendata$y,bnd)

# perform mds on the sample matrix
# options needed for insertion to work
full.mds<-cmdscale(D.full)


x.err<-c()
y.err<-c()

samp.size<-250

for(i in 1:1000){
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
   samp.mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)
   
   # mapped sample data
   samp.data<-list(x=c(),y=c())
   samp.data$x<-samp.mds$points[,1]
   samp.data$y<-samp.mds$points[,2]
   
   ### create prediction data
   # non-mapped prediction data
   npred.data<-list(x=c(),y=c())
   npred.data$x<-gendata$x[-samp.ind]
   npred.data$y<-gendata$y[-samp.ind]
   
   # new MDS coords for the prediction points
   pred.mds<-insert.mds(npred.data,gendata.samp,samp.mds,bnd)
   
   # put this in the correct format 
   pred.data<-list(x=rep(0,length(gendata$x)),y=rep(0,length(gendata$x)))
   pred.data$x[samp.ind]<-samp.data$x  # need to add in the sample points too
   pred.data$x[-samp.ind]<-pred.mds[,1]
   pred.data$y[samp.ind]<-samp.data$y  # need to add in the sample points too
   pred.data$y[-samp.ind]<-pred.mds[,2]

   x.err<-c(x.err,mean((pred.data$x-full.mds[,1])^2))
   y.err<-c(y.err,mean((pred.data$y-full.mds[,2])^2))

}




