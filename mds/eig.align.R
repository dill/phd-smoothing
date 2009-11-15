# align an MDS insertion to the original eigenvectors
eig.align<-function(X.f,X.s){
   # X.f is the configuration you want to align TO
   # X.s is the configuration you want to align

   # mean centre it
   X.f[,1]<-X.f[,1]-mean(X.f[,1])
   X.f[,2]<-X.f[,2]-mean(X.f[,2])
   X.f<-t(X.f)

   # and for the sample
   X.s[,1]<-X.s[,1]-mean(X.s[,1])
   X.s[,2]<-X.s[,2]-mean(X.s[,2])
   X.s<-t(X.s)

   # eigne decomposition of X.f X.f^T
   X.eig<-eigen(X.f%*%t(X.f))$vectors
   # re-align
   X.or<-t(X.eig)%*%X.s
   # return
   return(list(X.f=t(X.f),X.s=t(X.s)))
}

