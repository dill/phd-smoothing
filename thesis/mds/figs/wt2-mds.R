# wt2 MDS diagram

source("mds.R")
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

# data
gendata <- read.csv("wt2truth.csv",header=TRUE)
# squidge in another column that gets missed
xx<-c(rep(min(gendata$x-0.1326531),50),gendata$x)
yy<-c(seq(min(gendata$y),max(gendata$y),len=50),gendata$y)
inside<-inSide(bnd,xx,yy)
gendata<-data.frame(x=xx,y=yy)

onoff<-inSide(bnd,gendata$x,gendata$y)
gendata<- list(x=gendata$x[onoff],
               y=gendata$y[onoff],
               z=gendata$z[onoff])

# find the distances
D<-create_distance_matrix(gendata$x,gendata$y,bnd)

# 2d case
mds2<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=2)

pdf("wt2-2d-proj.pdf",width=5,height=2.5)
## plot it
par(mfrow=c(1,2),las=1,mar=c(2.5,2.5,1.8,1.5),cex.axis=0.5,cex.lab=0.75,mgp=c(1.5,0.75,0))
plot(gendata$x,gendata$y,asp=1,pch=".",xlab="x",ylab="y",cex=0.7)
lines(bnd,lwd=2)
plot(mds2$points[,1],mds2$points[,2],asp=1,pch=".",xlab="x*",ylab="y*",cex=0.7)
dev.off()

# 3d case
mds3<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=3)

pdf("wt2-3d-proj.pdf",width=6,height=2)
## plot it
par(mfrow=c(1,3),las=1,mar=c(2.5,2.5,1.8,1.5),mgp=c(1.5,0.75,0))#,cex.axis=0.3)
plot(mds3$points[,1],mds3$points[,2],asp=1,pch=".",xlab="x*",ylab="y*",cex=0.7)
plot(mds3$points[,2],mds3$points[,3],asp=1,pch=".",xlab="y*",ylab="z*",cex=0.7)
plot(mds3$points[,1],mds3$points[,3],asp=1,pch=".",xlab="x*",ylab="z*",cex=0.7)
dev.off()

