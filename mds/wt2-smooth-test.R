library(soap)

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

len<-length(gendata$x)

gendata$x<-gendata$x[seq(1,len,2)]
gendata$y<-gendata$y[seq(1,len,2)]
gendata$z<-gendata$z[seq(1,len,2)]

#plot(gendata$x,gendata$y)

D<-create_distance_matrix(gendata$x,gendata$y,bnd,logfile="wt2-logfile.txt")




# construct the distance matrix
D<-read.csv(file="wt2-logfile.txt",header=T)
D<-D[,-1]
D<-D+t(D)

# do the PCO and construct the data frame
new.coords<-cmdscale(D)
data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=gendata$z)






##########################################

samp.ind<-sample(1:length(gendata$x),100)


# mapped sample data
samp.data<-list(x=c(),y=c(),z=c())
samp.data$x<-data.mapped$x[samp.ind]
samp.data$y<-data.mapped$y[samp.ind]
samp.data$z<-data.mapped$z[samp.ind]

# non-mapped sample data
nsamp.data<-list(x=c(),y=c(),z=c())
nsamp.data$x<-gendata$x[samp.ind]
nsamp.data$y<-gendata$y[samp.ind]
nsamp.data$z<-gendata$z[samp.ind]




### mapping
b.mapped<-gam(z~s(x,y,k=49),data=samp.data)
fv <- predict(b.mapped,newdata=data.mapped)


# create the image
gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
ind<-c(1:length(gendata.ind$x))
pred.mat<-rep(NA,length(gendata.ind$x))

ind<-ind[gendata.ind$inside==1]
na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
ind<-ind[na.ind]
ind<-ind[onoff]
ind<-ind[seq(1,len,2)]

pred.mat[ind]<-fv

pred.mat<-matrix(pred.mat,50,50)

image(pred.mat)


### normal tprs
b.tprs<-gam(z~s(x,y,k=49),data=nsamp.data)
fv.tprs <- predict(b.tprs,newdata=gendata)




