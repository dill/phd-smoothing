# Plot the fit on the horseshoe using a thin plate spline
# to illustrate the leakage.


library(soap)

# output to pdf
pdf("leakageexample.pdf",4,4)


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

## fit 
b <- gam(y~s(v,w,k=40,bs="tp"),knots=knots)

## Plot the true function...
m<-300;n<-150
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

# predict
fv <- predict(b,newdata=data.frame(v=xx,w=yy))

# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names

# just get the bits in the horseshoe
fv[!insiders]<-NA

# plot
image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="",ylab="",
      main="",axes=FALSE,asp=1)
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)
# add a line around the edge
lines(fsb[[1]]$v,fsb[[1]]$w,lwd=3)

dev.off()

