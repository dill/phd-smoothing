# replacement of the mahalanobis() function in R
# automatically does P-M pseudo-inverse when p>n

mahalanobis<-function (x, center, cov=NULL, inverted = FALSE, ...){

   x<-if (is.vector(x)) 
         matrix(x, ncol = length(x))
      else as.matrix(x)

   if(nrow(x)>=ncol(x) & !is.null(cov)){
      # use the function in the stats package
      res<-stats::mahalanobis(x,center,cov,inverted,...)
   }else{

      # column centred t(x) 
      xc<-t(sweep(x,2,colMeans(x)))
      
      y <- sweep(x, 2, center)# = (x - center)
      y<-t(y)


      # find the singular value decomposition
      xc.svd<-svd(xc)

      # calculate the inverse of the covariance matrix
      #cov.inv<-(1/nrow(x)) * xc.svd$u%*%(diag(1/xc.svd$d)^2)%*%t(xc.svd$u)
   
      # only use non-"zero" elements of d, as in MASS::ginv()
      #nz<-xc.svd$d>1e-10
      nz<-xc.svd$d > .Machine$double.eps * xc.svd$d[1]
      Z<-(t(xc.svd$u[,nz])%*%y)/xc.svd$d[nz]

      # or not
      #Z<-(t(xc.svd$u)%*%y)/xc.svd$d

      #res<-rowSums((t(Z)%*%Z))/nrow(x)
      res<-rowMeans((t(Z)%*%Z))

   }

   return(res)
}

