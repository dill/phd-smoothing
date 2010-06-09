# plot within area distance against Euclidean distance in the MDS
# compare insertion against full
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

### do the full MDS
D.full<-create_distance_matrix(gendata$x,gendata$y,bnd)
full.mds<-cmdscale(D.full)


### now for a sample
samp.size<-250
samp.ind<-sample(1:length(gendata$x),samp.size)
gendata.samp<- list(x=gendata$x[samp.ind],
                    y=gendata$y[samp.ind],
                    z=gendata$z[samp.ind])
D<-create_distance_matrix(gendata.samp$x,gendata.samp$y,bnd)
samp.mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)
gendata.pred<- list(x=gendata$x[-samp.ind],
                    y=gendata$y[-samp.ind],
                    z=gendata$z[-samp.ind])
pred.mds<-insert.mds(gendata.pred,gendata.samp,samp.mds,bnd)


samp.full<-matrix(NA,length(gendata$x),2)

samp.full[samp.ind,1] <-samp.mds$points[,1]
samp.full[samp.ind,2] <-samp.mds$points[,2]
samp.full[-samp.ind,1]<-pred.mds[,1]
samp.full[-samp.ind,2]<-pred.mds[,2]

### now find the Euclidean distances within the MDS
# for the full MDS
full.euc<-as.matrix(dist(full.mds))
# for the sample
samp.euc<-as.matrix(dist(samp.full))


par(mfrow=c(1,2))

plot(x=full.euc[upper.tri(full.euc)],y=D.full[upper.tri(D.full)],main="full MDS",xlab="MDS distance",ylab="within-area distance",asp=1,pch=19,cex=0.3)
abline(0,1)
plot(x=samp.euc[upper.tri(samp.euc)],y=D.full[upper.tri(D.full)],main="sample MDS",xlab="MDS distance",ylab="within-area distance",asp=1,pch=19,cex=0.3)
abline(0,1)





