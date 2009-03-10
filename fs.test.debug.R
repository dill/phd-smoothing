### Debug version of fs.test that gives the a and d
#   coordinates as well as the function evaluation.
fs.test.debug <- function(x,y,r0=.1,r=.5,l=3,b=1,exclude=TRUE)
## test function based on Tim Ramsay (2002) J.R.Statist. Soc. B
## 64(2):307-319 "Spline smoothing over difficult regions"
{ q <- pi*r/2 ## 1/2 length of semi-circle part of centre curve
  a <- d <- x*0 ## along and distance to arrays

  ## convert x,y to along curve and distance to curve (a,d) 
  ## co-ordinates. 0 distance along is at (x=-r,y=0)  

  ind <- x>=0 & y>0
  a[ind] <- q + x[ind]
  d[ind] <- y[ind]-r

  ind <- x>=0 & y<=0
  a[ind] <- -q - x[ind]
  d[ind] <- -r - y[ind]


  ind <- x < 0
  a[ind] <- -atan(y[ind]/x[ind])*r
  d[ind] <- sqrt(x[ind]^2+y[ind]^2) - r

  ## create exclusion index

  ind <- abs(d)>r-r0 | (x>l & (x-l)^2+d^2 > (r-r0)^2)

 # f <- a*b # the original
  f <- a*b+d^2

  if (exclude) f[ind] <- NA
  attr(f,"exclude") <- ind
  return(list(f=f,a=a,d=d))
}

