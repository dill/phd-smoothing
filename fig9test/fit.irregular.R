# Fit a gam to the irregular region once it's been mapped

### load some data
# first the mapped points
irreg.data<-read.csv("fig9mapped.csv",header=FALSE)
# now the z values
z.vals<-read.csv("fig9out.csv",header=TRUE)


# merge
fit.data<-data.frame(x=irreg.data[,1],y=irreg.data[,2],z=z.vals$z)

# run mgcv
library(mgcv)
b.mapped<-gam(z~s(x)+s(y),data=fit.data)


#### predict back
# load some data...
true.vals<-read.csv("fig9truth.csv",header=TRUE)
true.vals.mapped<-read.csv("fig9truemapped.csv",header=FALSE)
names(true.vals.mapped)<-c("x","y")

# try to predict over the whole domain
fv <- predict(b.mapped,newdata=data.frame(x=true.vals.mapped$x,y=true.vals.mapped$y))




par(mfrow=c(2,2))

### first truth
image(surf,col=heat.colors(100),xlab="v",ylab="w",main="truth")
contour(surf,add=T)

### sc prediction w. tprs 
# do some faffing for the plots
pred.grid<-matrix(c(0),1000,1000)
pred.grid[inside.points]<-fv
pred.grid[!inside.points]<-NA

image(pred.grid,col=heat.colors(100),xlab="v",ylab="w",main="sc+tprs prediction")
contour(pred.grid,add=T)

### normal tprs
b.tprs<-gam(z~s(x)+s(y),data=z.vals)
fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x,y=true.vals$y))

pred.grid.tprs<-matrix(c(0),1000,1000)
pred.grid.tprs[inside.points]<-fv.tprs
pred.grid.tprs[!inside.points]<-NA
image(pred.grid.tprs,col=heat.colors(100),xlab="v",ylab="w",main="tprs prediction")
contour(pred.grid.tprs,add=T)

