# is my mahalanobis the same as the ginv one?


library(mdspack)

x<-matrix(rnorm(100000,sd=100),100,1000)

mah1<-mahalanobis(x,x[1,])

source("ginv-leuk.R")

mah2<-mahalanobis(x,x[1,])



#load("leuk.RData")
#leuk.num<-matrix(as.numeric(as.matrix(leuk)),nrow(leuk),ncol(leuk))
#ind<-colSums(is.na(leuk.num))>0
#leuk.t<-leuk.num[,c(1:ncol(leuk.num))[!ind]]
#
#
#system.time(mah2<-mahalanobis(leuk.t,leuk.t[1,]))
