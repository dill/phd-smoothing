
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
predback.real<-read.csv("./matlab/preal.csv",header=F)
predback.imag<-read.csv("./matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names


# first take a line that runs along the centre of the horseshoe
# can do thi using the transform in fs.test

# this line is equiv. to...

# doing the inverse to find the line
ramsay.inv <- function(a,d,r0=.1,r=.5,l=3,b=1,exclude=TRUE)
## test function based on Tim Ramsay (2002) J.R.Statist. Soc. B
## 64(2):307-319 "Spline smoothing over difficult regions"
{ 
  # does this make sense?
  q <- pi*r/2 ## 1/2 length of semi-circle part of centre curve
  x <- y <- d*0 ## along and distance to arrays

  ## convert x,y to along curve and distance to curve (a,d) 
  ## co-ordinates. 0 distance along is at (x=-r,y=0)  

  # this is the top arm
  ind <- a>=0.75
  x[ind] <- a[ind]-q
  y[ind] <- d[ind]+r

  # this is the bottom arm
  ind <- a<=-0.75 
  x[ind] <- -(q + a[ind]) 
  y[ind] <- -(r + d[ind])

  # this is the curve
  ind <- a < 0.75 & a>-0.75 #just a between those limits?
  x[ind]<-(d-r)*cos(-a/r)
  y[ind]<-(d-r)*sin(-a/r)

  ## create exclusion index
  # can probably ignore this
  ind <- abs(d)>r-r0 | (x>l & (x-l)^2+d^2 > (r-r0)^2)

 # f <- a*b # the original
  f <- a*b+d^2 # what is in by default

  if (exclude) f[ind] <- NA
  attr(f,"exclude") <- ind
# just for testing
  par(mfrow=c(1,2))
  plot(a,d)
  plot(x,y)
  return(list(f=f,a=a,d=d))
}



# take those points, map them

# evaluate the pointsusing fs.test


# linear fit of x,y against f.





#linear.fit<-lm(tru~prediction.grid$v*prediction.grid$w)
linear.fit<-lm(tru~prediction.grid$v+prediction.grid$w)



#pred<-predict(linear.fit,prediction.grid)
#pred[!insiders]<-NA


# predict with w set and vary v?
# then the other way around?








#put the predicted linear points z values onto the horseshoe




image(matrix(pred,m,n),col=heat.colors(100))
contour(matrix(pred,m,n),nlevels=50,add=TRUE)






