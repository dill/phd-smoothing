# test of lines across the domain for wt2
# run from phd-smoothing/mds
source("mds.R")
library(soap)


## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

res<-74

## Simulate some fitting data, inside boundary...
#gendata <- read.csv("wt2truth.csv",header=TRUE)
gendata<-list(x=c(),y=c(),inside=c())
m<-res;n<-res
xm <- seq(-3,3.5,length=m);yn<-seq(-3,3,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
onoff2<-inSide(list(x=-bnd$x,y=-bnd$y),-xx,-yy)

gendata$x<-xx
gendata$y<-yy
gendata$inside<-onoff&onoff2

###################
# create the grid
horiz.set<-c()
hind<-c()
for(i in 1:19){
   horiz.set<-c(horiz.set,(1:res+(res*i*5)))
   hind<-c(hind,rep(i,res))
}

vert.set<-c()
vind<-c()
for(i in 1:res){
   vert.set<-c(vert.set,seq(1,res,5)+res*i)
   vind<-c(vind,seq(1,15,1))
}


hgrid.set<-list(x=gendata$x[horiz.set],
                y=gendata$y[horiz.set],
                ind=hind)

vgrid.set<-list(x=gendata$x[vert.set],
                y=gendata$y[vert.set],
                ind=vind)



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


# setup the index for the horizontal grid
# this makes sure that all the lines are plotted separately
di<-diff(hgrid.set$x)
ju<-rep(0,length(di)+1)
k<-1
for(i in 1:length(di)){
   ju[i]<-k
#   if(abs(di[i]-0.1326531)>1e-5){
   if(abs(di[i]- 0.0890411)>1e-5){
      k<-k+1
   }
}
ju[length(ju)]<-k
hgrid.set$ind<-ju

# just a quick hack for the one vertical line that crosses a gap
ju<-vgrid.set$ind
ju[vgrid.set$ind==3][34:39]<-rep(max(ju)+1,6)
ju[vgrid.set$ind==6][38:47]<-rep(max(ju)+1,10)
vgrid.set$ind<-ju


## code to plot the unmorphed grid
#plot(gendata,asp=1,type="n",main="",xlab="",ylab="")
#for(i in unique(hgrid.set$ind)){
#   if(length(hgrid.set$x[hgrid.set$ind==i])==2){
#      points(x=hgrid.set$x[hgrid.set$ind==i],
#             y=hgrid.set$y[hgrid.set$ind==i],pch=19,cex=0.3)
#   }else{
#      lines(x=hgrid.set$x[hgrid.set$ind==i],
#             y=hgrid.set$y[hgrid.set$ind==i],pch=19,cex=0.3)
#   }
#}
#for(i in 1:max(vgrid.set$ind,na.rm=TRUE)){
#      lines(x=vgrid.set$x[vgrid.set$ind==i],
#             y=vgrid.set$y[vgrid.set$ind==i],pch=19,cex=0.3)
#}
#lines(bnd,lwd=2)

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

## code to plot the grid
par(mfrow=c(2,2))
# plot
plot(mds$points,asp=1,type="n",main="",xlab="",ylab="")

for(i in unique(hgrid.set$ind)){
   if(length(h.full.mds[hgrid.set$ind==i,])==2){
      points(x=h.full.mds[hgrid.set$ind==i,1],
             y=h.full.mds[hgrid.set$ind==i,2],pch=19,cex=0.3)
   }else{
      lines(h.full.mds[hgrid.set$ind==i,])
   }
}

for(i in 1:max(vgrid.set$ind,na.rm=TRUE)){
      lines(v.full.mds[vgrid.set$ind==i,])
}




# zoom of plot
plot(mds$points,asp=1,type="n",main="",xlim=c(2,4),ylim=c(-0.5,0.75),xlab="",ylab="")

for(i in unique(hgrid.set$ind)){
   if(length(h.full.mds[hgrid.set$ind==i,])==2){
      points(x=h.full.mds[hgrid.set$ind==i,1],
             y=h.full.mds[hgrid.set$ind==i,2],pch=19,cex=0.3)
   }else{
      lines(h.full.mds[hgrid.set$ind==i,])
   }
}

for(i in 1:max(vgrid.set$ind,na.rm=TRUE)){
      lines(v.full.mds[vgrid.set$ind==i,])
}


# now try with a sample

# create the sample
samp.size<-250


gendata.samp<- list(x=gendata$x[gendata$inside],
                    y=gendata$y[gendata$inside],
                    z=gendata$z[gendata$inside])

samp.ind<-sample(1:length(gendata.samp$x),samp.size)

gendata.samp<- list(x=gendata.samp$x[samp.ind],
                    y=gendata.samp$y[samp.ind],
                    z=gendata.samp$z[samp.ind])

### do the PCO and construct the data frame
# create D
D<-create_distance_matrix(gendata.samp$x,gendata.samp$y,bnd)

# perform mds on the sample matrix
samp.mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)

h.part.mds<-insert.mds(hgrid.set,gendata.samp,samp.mds,bnd)
v.part.mds<-insert.mds(vgrid.set,gendata.samp,samp.mds,bnd)


plot(x=samp.mds$points[,1],y=samp.mds$points[,2],asp=1,type="n",main="",xlab="",ylab="")

#plot(gendata,asp=1,type="n",main="",xlab="",ylab="")
#for(i in unique(hgrid.set$ind)){
#   if(length(hgrid.set$x[hgrid.set$ind==i])==2){
#      points(x=hgrid.set$x[hgrid.set$ind==i],
#             y=hgrid.set$y[hgrid.set$ind==i],pch=19,cex=0.3)
#   }else{
#      lines(x=hgrid.set$x[hgrid.set$ind==i],
#             y=hgrid.set$y[hgrid.set$ind==i],pch=19,cex=0.3)
#   }
#}
#for(i in 1:max(vgrid.set$ind,na.rm=TRUE)){
#      lines(x=vgrid.set$x[vgrid.set$ind==i],
#             y=vgrid.set$y[vgrid.set$ind==i],pch=19,cex=0.3)
#}



for(i in unique(hgrid.set$ind)){
   if(length(h.part.mds[hgrid.set$ind==i,])==2){
      points(x=h.part.mds[hgrid.set$ind==i,1],
             y=h.part.mds[hgrid.set$ind==i,2],pch=19,cex=0.3)
   }else{
      lines(x=h.part.mds[hgrid.set$ind==i,1],
            y=h.part.mds[hgrid.set$ind==i,2])
   }
}

for(i in 1:max(vgrid.set$ind,na.rm=TRUE)){
      lines(x=v.part.mds[vgrid.set$ind==i,1],
            y=v.part.mds[vgrid.set$ind==i,2])
}

# zoom of plot
plot(x=samp.mds$points[,1],y=samp.mds$points[,2],asp=1,type="n",main="",
     xlab="",ylab="",xlim=c(2,4),ylim=c(-0.5,0.75))

for(i in unique(hgrid.set$ind)){
   if(length(h.part.mds[hgrid.set$ind==i,])==2){
      points(x=h.part.mds[hgrid.set$ind==i,1],
             y=h.part.mds[hgrid.set$ind==i,2],pch=19,cex=0.3)
   }else{
      lines(x=h.part.mds[hgrid.set$ind==i,1],
            y=h.part.mds[hgrid.set$ind==i,2])
   }
}

for(i in 1:max(vgrid.set$ind,na.rm=TRUE)){
      lines(x=v.part.mds[vgrid.set$ind==i,1],
            y=v.part.mds[vgrid.set$ind==i,2])
}


