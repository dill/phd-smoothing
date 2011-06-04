# is my mahalanobis the same as the ginv one?


library(mdspack)
library(MASS)

x<-c()
for(i in 1:10){
   x<-cbind(x,rnorm(12,sd=10*runif(1),mean=i))
}


xc<-t(sweep(x,2,colMeans(x)))
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
#cov.inv<-xc.svd$u[,nz]%*%((((n-1)/xc.svd$d[nz])^2)*t(xc.svd$u[,nz]))
invd<-ginv(cov(x))-cov.inv





mah1<-stats::mahalanobis(x,x[1,],ginv(cov(x)),inverted=TRUE)
source("ginv-leuk.R")
mah2<-mahal2(x,x[1,])



#load("leuk.RData")
#leuk.num<-matrix(as.numeric(as.matrix(leuk)),nrow(leuk),ncol(leuk))
#ind<-colSums(is.na(leuk.num))>0
#leuk.t<-leuk.num[,c(1:ncol(leuk.num))[!ind]]
#
#
##system.time(mah2<-mahalanobis(leuk.t,leuk.t[1,]))
#source("ginv-leuk.R")
#tt<-system.time(mah3<-mahalanobis(leuk.t,leuk.t[1,]))
