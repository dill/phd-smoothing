library(soap)

# set sample size and noise level here
samp.size<-250
noise.level<-0.5



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


#D<-create_distance_matrix(gendata$x,gendata$y,bnd,logfile="wt2-logfile.txt")
# load the data rather than doing the computation
D<-read.csv(file="wt2-logfile.txt")
D<-D[,-1]


# do the PCO and construct the data frame
D<-D+t(D)


samp.ind<-sample(1:length(gendata$x),samp.size)

# take the sample of D
D.samp<-D[samp.ind,samp.ind]
# the rest : distances from those not in the sample 
# to those in the sample.
D.pred<-D[samp.ind,-samp.ind]

# perform mds on the sample matrix
# options needed for insertion to work
samp.mds<-cmdscale(D.samp,eig=TRUE,x.ret=TRUE)

# add noise
#> summary(gendata$z)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000236 0.269300 0.276300 0.479600 0.850000 
noise<-noise.level # set to zero for no noise
noise<-noise*rnorm(length(samp.ind))

# mapped sample data
samp.data<-list(x=c(),y=c(),z=c())
samp.data$x<-samp.mds$points[,1]
samp.data$y<-samp.mds$points[,2]
samp.data$z<-gendata$z[samp.ind]+noise

# non-mapped sample data
nsamp.data<-list(x=c(),y=c(),z=c())
nsamp.data$x<-gendata$x[samp.ind]
nsamp.data$y<-gendata$y[samp.ind]
nsamp.data$z<-gendata$z[samp.ind]+noise


rm(insert.mds)
source("insert.mds.R")

# non-mapped prediction data
npred.data<-list(x=c(),y=c(),z=c())
npred.data$x<-gendata$x[-samp.ind]
npred.data$y<-gendata$y[-samp.ind]
#npred.data$z<-gendata$z[-samp.ind]+noise

# check to see how it's gone
#par(mfrow=c(2,2))
#plot(nsamp.data$x,nsamp.data$y,asp=1,main="original domain")
#points(npred.data$x,npred.data$y,col="blue",pch=25)
#plot(samp.data$x,samp.data$y,asp=1,main="point by point")


#for (i in 1:dim(D.pred)[1]) {
#
#   pred.mds<-insert.mds(D.pred[,i],samp.mds)
#
#   # now do the insertion for the prediction points
#   pred.data<-list(x=c(),y=c())#,z=c())
#   pred.data$x<-c(pred.mds[1,])
#   pred.data$y<-c(pred.mds[2,])
#   #pred.data$z<-gendata$z[samp.ind]+noise
#
#
#   points(pred.data,col="blue",pch=25)
#}


#plot(samp.data$x,samp.data$y,asp=1,main="at once")
pred.mds<-insert.mds(D.pred,samp.mds)

# now do the insertion for the prediction points
pred.data<-list(x=rep(0,dim(D)[2]),y=rep(0,dim(D)[2]))
pred.data$x[samp.ind]<-samp.data$x  # need to add in the sample points too
pred.data$x[-samp.ind]<-pred.mds[1,]
pred.data$y[samp.ind]<-samp.data$y  # need to add in the sample points too
pred.data$y[-samp.ind]<-pred.mds[2,]
#points(pred.data,col="blue",pch=25)

#total.mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)
#pred.data<-list(x=c(),y=c())#,z=c())
#pred.data$x<-c(total.mds$points[,1])
#pred.data$y<-c(total.mds$points[,2])
#plot(pred.data$x,pred.data$y,asp=1,main="full mds")


##########################################


### mapping
b.mapped<-gam(z~s(x,y,k=49),data=samp.data)
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

# plot for truth, mds, tprs and soap
par(mfrow=c(2,2))

# axis scales
xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)



pred.mat[ind]<-gendata$z
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="truth",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
contour(xscale,yscale,pred.mat,add=T)

pred.mat[ind]<-fv
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="mds",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
contour(xscale,yscale,pred.mat,add=T)

pred.mat[ind]<-fv.tprs
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="tprs",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
contour(xscale,yscale,pred.mat,add=T)

pred.mat[ind]<-fv.soap
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="soap",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
contour(xscale,yscale,pred.mat,add=T)

### calculate MSEs
cat("mds MSE=",mean((fv-gendata$z)^2,na.rm=T),"\n")
cat("tprs MSE=",mean((fv.tprs-gendata$z)^2,na.rm=T),"\n")
cat("soap MSE=",mean((fv.soap-gendata$z)^2,na.rm=T),"\n")


#### MDS explanatory plot
par(mfrow=c(1,2))
red.points<-data.mapped$x < -2
green.points<-data.mapped$y>1 & data.mapped$x<0
blue.points<-data.mapped$y>-0.5 & data.mapped$x>2
orange.points<-gendata$y>-1 & gendata$x>-0.5 & gendata$x<1

plot(gendata$x,gendata$y,asp=1,las=1,xlab="x",ylab="y",cex=0.5,pch=20)
points(gendata$x[red.points],gendata$y[red.points],cex=0.5,pch=20,col="red")
points(gendata$x[green.points],gendata$y[green.points],cex=0.5,pch=20,col="green")
points(gendata$x[blue.points],gendata$y[blue.points],cex=0.5,pch=20,col="blue")
points(gendata$x[orange.points],gendata$y[orange.points],cex=0.5,pch=20,col="orange")

plot(data.mapped$x,data.mapped$y,asp=1,las=1,xlab="x",ylab="y",cex=0.5,pch=20)
points(data.mapped$x[red.points],data.mapped$y[red.points],cex=0.5,pch=20,col="red")
points(data.mapped$x[green.points],data.mapped$y[green.points],cex=0.5,pch=20,col="green")
points(data.mapped$x[blue.points],data.mapped$y[blue.points],cex=0.5,pch=20,col="blue")
points(data.mapped$x[orange.points],data.mapped$y[orange.points],cex=0.5,pch=20,col="orange")


dev.copy2pdf(file="wt2-mds-coloured.pdf")


