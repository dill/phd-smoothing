# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")
 
samp.size=250
samp.size=100

 
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
#bnd.neg<-list(x=-bnd$x,y=-bnd$y)
#onoff<-inSide(bnd.neg,-gendata$x,-gendata$y)

#gendata<-list(x=gendata$x[onoff],
#               y=gendata$y[onoff],
#               z=gendata$z[onoff])

# create the sample index
samp.ind<-sample(1:length(gendata$x),samp.size)

## create the sample
gendata.samp<- list(x=gendata$x[samp.ind],
                    y=gendata$y[samp.ind],
                    z=gendata$z[samp.ind])

gendata<-list(x=gendata$x[-samp.ind],
               y=gendata$y[-samp.ind],
               z=gendata$z[-samp.ind])

# create D
D.samp<-create_distance_matrix(gendata.samp$x,gendata.samp$y,bnd)

mymds<-function(D){

   n<-dim(D)[1]

   # squaring
   D<-D^2

   # double centre D
   ones<-t(t(rep(1,n)))
   H<-diag(1,n,n)-1/n*(ones%*%t(ones))

   S<- -1/2*H%*%D%*%H

   # eigen decompose
   eS<-eigen(S,symmetric=TRUE)
   U<-eS$vectors
   lambda<-eS$values[1:2]

   X<-U[,1:2]%*%diag(sqrt(eS$values[1:2]),2)

   return(list(points=X,eig=lambda))
}


#library(debug)
#mtrace(cmdscale)
#mtrace(insert.mds)
# perform mds on D
#samp.mds<-cmdscale(D.samp,eig=TRUE,k=2,x.ret=TRUE)
samp.mds<-mymds(D.samp)

# prediction points insertion
#pred.mds<-insert.mds(gendata,gendata.samp,samp.mds,bnd)


#par(mfrow=c(1,2))
plot(samp.mds$points,pch=19,cex=0.3,asp=1)


# sample points insertion
samp.ins<-insert.mds(gendata.samp,gendata.samp,samp.mds,bnd)

points(samp.ins,pch=19,cex=0.3,col="red")


#library(vegan)
#
#pp<-procrustes(samp.ins,samp.mds$points,scale=TRUE)
#pred.pp<-pred.mds
#pred.pp<-cbind(pred.pp[,1]-pp$translation[1,1],pred.pp[,2]-pp$translation[1,2])
#p.fix<-pred.pp%*%pp$rotation
#
#cat("translation=",pp$translation[1,1],pp$translation[1,2],"\n")
#plot(p.fix,pch=19,cex=0.3,asp=1)
#points(samp.mds$points,pch=19,cex=0.3,col="red")
