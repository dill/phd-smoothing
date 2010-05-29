# show the point mappings for wt2

# load in the unmapped data
wtu<-read.csv(file="wt2truth.csv",header=T)
# and the boundary
bnd<-read.csv(file="wt2-verts.csv",header=F)
names(bnd)<-c("x","y")

# load in the mapped data
wtm<-read.csv(file="wt2truemapped.csv",header=F)
names(wtm)<-c("x","y","z")

pdf(file="wt2-points.pdf",width=4,height=2)

par(mfrow=c(1,2),mar=c(4,4,2,2),pch=19,las=1,cex.lab=2,cex.axis=1,cex=0.3)

plot(wtu$x[!!wtu$inside],wtu$y[!!wtu$inside],asp=1,xlab="x",ylab="y")
lines(bnd,lwd=2)

plot(wtm$x,wtm$y,asp=1,xlab="x*",ylab="y*")
lines(x=c(max(wtm$x),min(wtm$x),min(wtm$x),max(wtm$x),max(wtm$x)),
      y=c(max(wtm$y),min(wtm$y),max(wtm$y),min(wtm$y),min(wtm$y)),lwd=2)

dev.off()
