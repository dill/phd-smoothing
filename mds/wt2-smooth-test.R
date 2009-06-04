source("mds.R")
source("wood.R")
source("utils.R")

## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE) 
names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...


gendata <- read.csv("wt2truth.csv",header=TRUE) 

gendata<- list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
onoff<-inSide(gendata$x,gendata$y,bnd.neg)

gendata<- list(x=gendata$x[onoff],
               y=gendata$y[onoff],
               z=gendata$z[onoff])


samp.ind<-sample(1:length(gendata$x),250)

x<-gendata$x[samp.ind]
y<-gendata$y[samp.ind]

D<-create_distance_matrix(x,y,bnd)

new.coords<-cmdscale(D)

data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=gendata$z[samp.ind])

### mapping
b.mapped<-gam(z~s(x,y,k=49),data=data.mapped)
fv <- predict(b.mapped,newdata=data.mapped)

### normal tprs
b.tprs<-gam(z~s(x,y,k=49),data=samp.data)
fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))




