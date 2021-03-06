# Fit a gam to the irregular region once it's been mapped

library(mgcv)
library(soap)


pdf(file="wigglytop2-bbox-real.pdf",width=4,height=4)

# load some data...
true.vals<-read.csv("wtbbtruth.csv",header=TRUE)
true.vals.mapped<-read.csv("wtbbtruemapped.csv",header=FALSE)
names(true.vals.mapped)<-c("x","y","z")
verts<-read.csv("figverts-real.csv",header=FALSE)
names(verts)<-c("x","y")
verts<-data.frame(x=c(verts$x,verts$x[1]),y=c(verts$y,verts$y[1]))

# how many points to sample
samp.size<-500

# make a sample index
this.sample<-sample(c(1:dim(true.vals)[1]),samp.size)

# noise
ran<-rnorm(samp.size)*0.02

true.vals$z[true.vals$inside==0]<-NA

# take the points from the true and true mapped
samp.data<-data.frame(x=true.vals$x[this.sample],y=true.vals$y[this.sample],z=true.vals$z[this.sample]+ran)
samp.data.mapped<-data.frame(x=true.vals.mapped$x[this.sample],y=true.vals.mapped$y[this.sample],z=true.vals.mapped$z[this.sample]+ran)


res<-sqrt(length(true.vals$x))
par(mfrow=c(2,2),las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

#surf[!inside.points]<-NA

### first truth
tru<-matrix(c(0),res,res)
tru[true.vals$inside==1]<-true.vals$z[true.vals$inside==1]
tru[true.vals$inside==0]<-NA
image(tru,col=heat.colors(100),xlab="x",ylab="y",main="truth",asp=1)
contour(tru,add=T)

### sc prediction w. tprs 
b.mapped<-gam(z~s(x,y,k=49),data=samp.data.mapped)
# try to predict over the whole domain
fv <- predict(b.mapped,newdata=data.frame(x=true.vals.mapped$x,y=true.vals.mapped$y))
# do some faffing for the plots
pred.grid<-matrix(c(0),res,res)
pred.grid[true.vals$inside==1]<-fv
pred.grid[true.vals$inside==0]<-NA

image(pred.grid,col=heat.colors(100),xlab="x",ylab="y",main="sc+tprs",asp=1)
contour(pred.grid,add=T)

### normal tprs
b.tprs<-gam(z~s(x,y,k=49),data=samp.data)
fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))

pred.grid.tprs<-matrix(c(0),res,res)
pred.grid.tprs[true.vals$inside==1]<-fv.tprs
pred.grid.tprs[true.vals$inside==0]<-NA
image(pred.grid.tprs,col=heat.colors(100),xlab="x",ylab="y",main="tprs",asp=1)
contour(pred.grid.tprs,add=T)

### soap
# setup knots
knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
insideknots<-inSide(verts,knots.x,knots.y)
#insideknots[59]<-FALSE # for 10x10
#insideknots[158]<-FALSE;insideknots[56]<-FALSE;insideknots[141]<-FALSE # for 15x15
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
knots<-data.frame(x=knots$x[-c(55,96,108)],y=knots$y[-c(55,96,108)])
boundary.knots<-49
#plot(verts,type="l");points(knots,col="red")

## get only the inside points
inside.points<-inSide(verts,samp.data$x,samp.data$y)
samp.data<-data.frame(x=samp.data$x[inside.points],y=samp.data$y[inside.points],z=samp.data$z[inside.points])

# fit
b.soap<-gam(z~s(x,y,bs="so",xt=list(bnd=list(verts)),k=boundary.knots),data=samp.data,knots=knots)
fv.soap <- predict(b.soap,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))

pred.grid.soap<-matrix(c(0),res,res)
pred.grid.soap[true.vals$inside==1]<-fv.soap
pred.grid.soap[true.vals$inside==0]<-NA
image(pred.grid.soap,col=heat.colors(100),xlab="x",ylab="y",main="soap",asp=1)
contour(pred.grid.soap,add=T)


### calculate the MSEs

cat("sc+tprs",mean((true.vals$z[true.vals$inside==1]-fv)^2,na.rm=T),"\n")
cat("tprs",mean((true.vals$z[true.vals$inside==1]-fv.tprs)^2,na.rm=T),"\n")
cat("soap",mean((true.vals$z[true.vals$inside==1]-fv.soap)^2,na.rm=T),"\n")

dev.off()
