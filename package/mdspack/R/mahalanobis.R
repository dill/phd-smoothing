# replacement of the mahalanobis() function in R
# automatically does P-M pseudo-inverse when p>n

mahalanobis<-function (x, center, cov=NULL, inverted = FALSE, xc.svd=NULL, ...){

   x<-if (is.vector(x)) 
         matrix(x, ncol = length(x))
      else as.matrix(x)

   if(nrow(x)>=ncol(x) & !is.null(cov)){
      # use the function in the stats package
      res<-stats::mahalanobis(x,center,cov,inverted,...)
   }else{
      # number of data
      n<-nrow(x)

      # column centred t(x) 
      xc<-t(sweep(x,2,colMeans(x)))
      
      y <- sweep(x, 2, center)# = (x - center)
      y<-t(y)

      # find the singular value decomposition (if we need to)
      if(is.null(xc.svd)){
         xc.svd<-svd(xc)
      }

      # only use non-"zero" elements of d, as in MASS::ginv()
      nz<-xc.svd$d > (sqrt(.Machine$double.eps) * xc.svd$d[1])

      # calculate the inverse of the covariance matrix
      #cov.inv<-xc.svd$u[,nz]%*%(((1/xc.svd$d[nz])^2)*t(xc.svd$u[,nz]))*(n-1)
      # calculate the Mahalanobis distance
      #res<-t(y)%*%cov.inv%*%y
      #res<-colMeans(res)

      # quick way of doing the above
      Z<-t(y)%*%xc.svd$u[,nz]%*%diag(sqrt(n-1)/xc.svd$d[nz])
      res<-colSums(Z%*%t(Z))

      # attach the svd to the result, to save time later
      attr(res,"xc.svd")<-xc.svd
   }
   return(res)
}
