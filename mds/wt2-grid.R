# test of lines across the domain for wt2
source("mds.R")

## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata <- read.csv("wt2truth.csv",header=TRUE)



horiz.set<-c()
for(i in 0:9){
   horiz.set<-c(horiz.set,(1:50)+(50*i*5))
}

vert.set<-c()
for(i in 1:50){
   vert.set<-c(vert.set,seq(1,50,5)+50*i)
}


grid.set<-list(x=gendata$x[unique(c(vert.set,horiz.set))],
               y=gendata$y[unique(c(vert.set,horiz.set))],
               z=gendata$z[unique(c(vert.set,horiz.set))],
               inside=gendata$inside[unique(c(vert.set,horiz.set))])


grid.set<- list(x=grid.set$x[grid.set$inside==1],
                y=grid.set$y[grid.set$inside==1],
                z=grid.set$z[grid.set$inside==1])





na.ind<-!(is.na(grid.set$x)&is.na(grid.set$y))

grid.set<- list(x=grid.set$x[na.ind],
               y=grid.set$y[na.ind],
               z=grid.set$z[na.ind])



gendata<- list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

### do the full thing

# create D
D<-create_distance_matrix(gendata$x,gendata$y,bnd)

# perform mds
mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)

# plot
full.mds<-insert.mds(grid.set,gendata,mds,bnd)
plot(full.mds,asp=1)


# 3d
library(rgl)
mds3<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=3)
pred.mds3<-insert.mds(grid.set,gendata,mds3,bnd)
open3d()
plot3d(pred.mds3[,1],pred.mds3[,2],pred.mds3[,3],size=2)


# now try with a sample


# create the sample
samp.size<-250
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

samp.data<-list(x=c(),y=c(),z=c())
samp.data$x<-samp.mds$points[,1]
samp.data$y<-samp.mds$points[,2]
samp.data$z<-gendata$z[samp.ind]

pred.mds<-insert.mds(grid.set,gendata.samp,samp.mds,bnd)











