# is my mahalanobis the same as the ginv one?


library(mdspack)
library(MASS)

x<-c()
for(i in 1:100){
   x<-cbind(x,rnorm(40,sd=10*runif(1),mean=10*runif(1)))
}


xc<-sweep(t(x),2,colMeans(t(x)))
n<-nrow(x)

# check that the covariance is calculated the same way
#covd<-cov(x)-xc%*%t(xc)/(n-1) # TICK

xc.svd<-svd(xc)
nz<-xc.svd$d>sqrt(.Machine$double.eps)*xc.svd$d[1]

# cov(x) is the same as it's SVD version
#covsvdcheck<-cov(x)-
#         (xc.svd$u[,nz]%*%diag(xc.svd$d[nz])^2%*%t(xc.svd$u[,nz]))/(n-1) # TICK

# check that the inverses are the same
cov.inv<-xc.svd$u[,nz]%*%(((1/xc.svd$d[nz])^2)*t(xc.svd$u[,nz]))*(n-1)
#invd<-ginv(cov(x))-cov.inv

# check the mahalanobis distances
y <- sweep(x, 2, x[1,])# = (x - center)
y<-t(y)

m<-rowSums((t(y)%*%cov.inv)*t(y))

t(x[1,]-x[2,])%*%ginv(cov(x))%*%(x[1,]-x[2,])
#
mah1<-stats::mahalanobis(x,x[1,],ginv(cov(x)),inverted=TRUE)
##source("mah.R")
mah2<-mahalanobis(x,x[1,])



#load("leuk.RData")
#leuk.num<-matrix(as.numeric(as.matrix(leuk)),nrow(leuk),ncol(leuk))
#ind<-colSums(is.na(leuk.num))>0
#leuk.t<-leuk.num[,c(1:ncol(leuk.num))[!ind]]
#
#
##system.time(mah2<-mahalanobis(leuk.t,leuk.t[1,]))
#source("ginv-leuk.R")
#tt<-system.time(mah3<-mahalanobis(leuk.t,leuk.t[1,]))
