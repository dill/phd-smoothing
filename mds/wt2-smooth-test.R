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

D<-create_distance_matrix(gendata$x,gendata$y,bnd)






new.coords<-cmdscale(D)
data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=z)






##########################################

samp.ind<-sample(1:length(gendata$x),100)

x<-gendata$x[samp.ind]
y<-gendata$y[samp.ind]
z<-gendata$z[samp.ind]


# write this out to file, so we don't have to do it again
#write.csv(data.mapped,file="wt2-mapped.csv")
#write.csv(cbind(x,y,z),file="wt2-unmapped.csv")



### mapping
b.mapped<-gam(z~s(x,y,k=49),data=data.mapped)
fv <- predict(b.mapped,newdata=data.mapped)





### normal tprs
#b.tprs<-gam(z~s(x,y,k=49),data=samp.data)
#fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))




