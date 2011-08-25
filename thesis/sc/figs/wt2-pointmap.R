# show the point mappings for wt2

library(soap)

# load in the unmapped data
wtu<-read.csv(file="wt2truth.csv",header=T)
# and the boundary
bnd<-read.csv(file="wt2-verts.csv",header=F)
names(bnd)<-c("x","y")


# squidge in another column that gets missed
xx<-c(rep(min(wtu$x-0.1326531),50),wtu$x)
yy<-c(seq(min(wtu$y),max(wtu$y),len=50),wtu$y)
inside<-inSide(bnd,xx,yy)
wtu<-data.frame(x=xx,y=yy,inside=inside)

# load in the mapped data
wtm<-read.csv(file="wt2truemapped.csv",header=F)
names(wtm)<-c("x","y","z")

pdf(file="wt2-points.pdf",width=5,height=2.5)

# bltr
par(mfrow=c(1,2),las=1,cex.lab=0.7,cex.axis=0.7,mar=c(3,2.5,1,1),mgp=c(1.8,0.7,0))

plot(wtu$x[wtu$inside],wtu$y[wtu$inside],asp=1,xlab="x",ylab="y",cex=0.2,pch=19)
lines(bnd,lwd=2)

plot(wtm$x,wtm$y,asp=1,xlab="x*",ylab="y*",cex=0.2,pch=19)
lines(x=c(max(wtm$x),min(wtm$x),min(wtm$x),max(wtm$x),max(wtm$x)),
      y=c(max(wtm$y),max(wtm$y),min(wtm$y),min(wtm$y),max(wtm$y)),lwd=2)

dev.off()

#plot(wtm$x,wtm$y,asp=1,xlab="x*",ylab="y*",type="n")
#
#for(i in 1:51){
#   x<-wtm$x[i:(i+50)]
#   y<-wtm$y[i:(i+50)]
#   ind<-inside[i:(i+50)]
#   lines(x[ind],y[ind])
#}
#
