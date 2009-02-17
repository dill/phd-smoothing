
library(soap)


# need to create the prediction grid here!!!


fsb <- list(fs.boundary())


# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- fs.test(xx,yy) 

# read in the predicition grid from matlab
predback.real<-read.csv("../matlab/preal.csv",header=F)
predback.imag<-read.csv("../matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names


#linear.fit<-lm(tru~prediction.grid$v*prediction.grid$w)
linear.fit<-lm(tru~prediction.grid$v+prediction.grid$w)
pred<-predict(linear.fit,prediction.grid)
pred[!insiders]<-NA
image(matrix(pred,m,n),col=heat.colors(100))
contour(matrix(pred,m,n),nlevels=50,add=TRUE)






