# test of lines across the domain for wt2
source("mds.R")
library(soap)


## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

res<-100

## Simulate some fitting data, inside boundary...
#gendata <- read.csv("wt2truth.csv",header=TRUE)
gendata<-list(x=c(),y=c(),inside=c())
m<-res;n<-res
xm <- seq(-3,3.5,length=m);yn<-seq(-3,3,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
gendata$x<-xx
gendata$y<-yy
gendata$inside<-onoff

###################
# create the grid
horiz.set<-c()
hind<-c()
for(i in 0:19){
   horiz.set<-c(horiz.set,(1:res+(res*i*5)))
   hind<-c(hind,rep(i,res))



}

vert.set<-c()
vind<-c()
for(i in 1:res){
   vert.set<-c(vert.set,seq(1,res,20)+res*i)
   vind<-c(vind,seq(1,20,1))
}


hgrid.set<-list(x=gendata$x[horiz.set],
                y=gendata$y[horiz.set],
                ind=hind)

vgrid.set<-list(x=gendata$x[vert.set],
                y=gendata$y[vert.set],
                ind=vind)


#par(mfrow=c(1,2))
#plot(hgrid.set)
#plot(vgrid.set)



na.ind<-!(is.na(vgrid.set$x)&is.na(vgrid.set$y))
vgrid.set<-list(x=vgrid.set$x[na.ind],
                y=vgrid.set$y[na.ind],
                z=vgrid.set$z[na.ind],
                ind=vgrid.set$ind[na.ind])

na.ind<-!(is.na(hgrid.set$x)&is.na(hgrid.set$y))
hgrid.set<-list(x=hgrid.set$x[na.ind],
                y=hgrid.set$y[na.ind],
                z=hgrid.set$z[na.ind],
                ind=hgrid.set$ind[na.ind])

onoff<-inSide(bnd=bnd,x=hgrid.set$x,y=hgrid.set$y)
hgrid.set<-list(x=hgrid.set$x[onoff],
                y=hgrid.set$y[onoff],
                z=hgrid.set$z[onoff],
                ind=hgrid.set$ind[onoff])

onoff<-inSide(bnd=bnd,x=vgrid.set$x,y=vgrid.set$y)
vgrid.set<-list(x=vgrid.set$x[onoff],
                y=vgrid.set$y[onoff],
                z=vgrid.set$z[onoff],
                ind=vgrid.set$ind[onoff])



##########################
# done

gendata<- list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

na.ind<-!(is.na(gendata$x)&is.na(gendata$y))

gendata<- list(x=gendata$x[na.ind],
               y=gendata$y[na.ind],
               z=gendata$z[na.ind])


####

### do the full thing
# create D
D<-create_distance_matrix(gendata$x,gendata$y,bnd)
# perform mds
mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)





h.full.mds<-insert.mds(hgrid.set,gendata,mds,bnd)
v.full.mds<-insert.mds(vgrid.set,gendata,mds,bnd)


# plot
plot(mds$points,asp=1,type="n")

for(i in 0:19){
   lines(h.full.mds[hgrid.set$ind==i,])
}


for(i in 1:50){
   lines(v.full.mds[vgrid.set$ind==i,])
}


# 3d
#library(rgl)
#mds3<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=3)
#pred.mds3<-insert.mds(grid.set,gendata,mds3,bnd)
#
## plot
#open3d()
#plot3d(pred.mds3[,1],pred.mds3[,2],pred.mds3[,3],size=2)


# now try with a sample

# create the sample
#samp.size<-250
#samp.ind<-sample(1:length(gendata$x),samp.size)
#
#gendata.samp<- list(x=gendata$x[samp.ind],
#                    y=gendata$y[samp.ind],
#                    z=gendata$z[samp.ind])
#
#### do the PCO and construct the data frame
## create D
#D<-create_distance_matrix(gendata.samp$x,gendata.samp$y,bnd)
#
## perform mds on the sample matrix
## options needed for insertion to work
#samp.mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)
#
#samp.data<-list(x=c(),y=c(),z=c())
#samp.data$x<-samp.mds$points[,1]
#samp.data$y<-samp.mds$points[,2]
#samp.data$z<-gendata$z[samp.ind]
#
#pred.mds<-insert.mds(grid.set,gendata.samp,samp.mds,bnd)
#
#
## for 2d plot full against the sample set of points
#par(mfrow=c(1,2))
#plot(full.mds,asp=1, main="full point set")
## seems like this flips, so negate it
#plot(pred.mds[,1],-pred.mds[,2],asp=1,main="sample point set")
#
## zoom
#par(mfrow=c(1,2))
#plot(full.mds,asp=1, main="full point set",xlim=c(1.5,4),ylim=c(0,1))
## seems like this flips, so negate it
#plot(pred.mds[,1],-pred.mds[,2],asp=1,main="sample point set",xlim=c(1.5,4),ylim=c(0,1))






