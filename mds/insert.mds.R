"insert.mds"<-function(new.dist,cmd.object){
   # insert a new point into the MDS, see Gower (Biometrika, 1968.)
   # args
   #  new.dists      new set of distances from the old points to new
   #                 should take the form of a 1 by n matrix, where there are
   #                 and n original points to measure to.
   #  cmd.object     object returned from cmdscale, with eig=TRUE
   # ret
   # coordinates of new points in the mds?
   
   # want to calculate 1/2 * lambda^(-1) * X' * d

   # first lambda
   nrow<-dim(cmd.object$points)[2]
   lambda<-matrix(0,nrow=nrow,ncol=nrow)
   diag(lambda)<-cmd.object$eig
   
   # find lambda^(-1)
   lambda.inverse<-solve(lambda)

   # take the original MDS coordinates and their transpose
   X<-cmd.object$points
   Xt<-t(X)
   
   # finally d
   # the ith element of d is d_i^2-d_{i,n+1}^2
   d<-diag(X%*%Xt)-new.dist^2

   # finally construct the product
   ret<-1/2*(lambda.inverse %*% Xt %*% d)

   # return the result
   return(ret)
}
