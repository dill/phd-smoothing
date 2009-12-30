# my MDS function
# DLM 2009
mymds<-function(D){

   n<-dim(D)[1]

   # squaring
   D<-D^2

   # double centre D
   ones<-t(t(rep(1,n)))
   H<-diag(1,n,n)-1/n*(ones%*%t(ones))

   S<- -1/2*H%*%D%*%H

   # eigen decompose
   eS<-eigen(S,symmetric=TRUE)
   U<-eS$vectors
   lambda<-eS$values[1:2]

   X<-U[,1:2]%*%diag(sqrt(eS$values[1:2]),2)

   return(list(points=X,eig=lambda,S=S))
}

