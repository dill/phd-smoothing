# Alternative version of the Ramsey horshoe with the
# gradient going across the short axis.

# modified from soap v 0.1, Simon Wood

ramsay.alt <- function(x,y,r0=.1,r=.5,l=3,b=1,exclude=TRUE)
## test function based on Tim Ramsay (2002) J.R.Statist. Soc. B
## 64(2):307-319 "Spline smoothing over difficult regions"
{ q <- pi*r/2 ## 1/2 length of semi-circle part of centre curve
  a <- d <- x*0 ## along and distance to arrays

  ## convert x,y to along curve and distance to curve (a,d) 
  ## co-ordinates. 0 distance along is at (x=-r,y=0)  

  # top arm
  ind <- x>=0 & y>0
  a[ind] <- q + x[ind]
  d[ind] <- y[ind]-r

  # bottom arm
  ind <- x>=0 & y<=0
  a[ind] <- -q - x[ind]
  d[ind] <- -r - y[ind]

  # curved bit
  ind <- x < 0
  a[ind] <- -atan(y[ind]/x[ind])*r
  d[ind] <- sqrt(x[ind]^2+y[ind]^2) - r

  ## create exclusion index
  ind <- abs(d)>r-r0 | (x>l & (x-l)^2+d^2 > (r-r0)^2)

 # f <- a*b # the original
#  f <- a*b+d^2 # what is in by default
  f<-abs(d)

  #f[a>4]<-seq(0,0.2,length.out=length(f[a>4]))

   tmp<-(a-4)^2
   #tmp[a<4 & a>-4]<-0
   f<-f+tmp

  if (exclude) f[ind] <- NA
  attr(f,"exclude") <- ind
# just for testing
  # plot(a,d)
  return(f)
}

