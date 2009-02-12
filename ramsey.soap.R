# Simon's code taken straight from soap-package.

library(soap)

## create a boundary...
fsb <- list(fs.boundary())
 
## create some internal knots...
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                    w=rep(c(-.6,-.3,.3,.6),rep(8,4)))

## Simulate some fitting data, inside boundary...
n<-1000
v <- runif(n)*5-1;w<-runif(n)*2-1
y <- fs.test(v,w,b=1)
ind <- inSide(fsb,x=v,y=w) ## remove outsiders
y <- y[ind];v <- v[ind]; w <- w[ind] 
n <- length(y)
y <- y + rnorm(n)*.3 ## add noise

par(mfrow=c(3,2))
## plot boundary with knot and data locations
plot(fsb[[1]],type="l");points(knots,pch=20,col=2)
points(v,w,pch=".");

names(fsb[[1]]) <- c("v","w") ## correct boundary names

## Now fit the soap film smoother...
b <- gam(y~s(v,w,k=40,bs="so",xt=list(bnd=fsb)),knots=knots)
plot(b) ## default plot
vis.gam(b,plot.type="contour") ## nicer visualization

## Plot the true function...
m<-100;n<-50 
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

tru <- matrix(fs.test(xx,yy),m,n) ## truth
image(xm,yn,tru,col=heat.colors(100),xlab="v",ylab="w",
      main="truth")
lines(fsb[[1]],lwd=3)
contour(xm,yn,tru,levels=seq(-5,5,by=.25),add=TRUE)

## Do a known boundary example (note no `k' needed)
## First define the value for the smooth at each 
## supplied boundary point...
fsb[[1]]$f <- fs.test(fsb[[1]]$v,fsb[[1]]$w,b=1,exclude=FALSE)

## Now fit the smooth...
bk <- gam(y~s(v,w,bs="so",xt=list(bnd=fsb)),knots=knots)
plot(bk) ## default plot

## Produce a prettier plot of the fit, by predicting on a fine grid...
fv <- predict(bk,newdata=data.frame(v=xx,w=yy),block.size=-1)
## ... `block.size=-1' stops `predict.gam' from splitting `newdata'
## in to several chuncks for predicting: this would involve expensive
## duplication of PDE solving.

## The actual plot...
image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="v",ylab="w",
      main="known boundary soap film")
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)
