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

   # find lambda^(-1)
   lambda.inverse<-1/cmd.object$eig

   # take the original MDS coordinates and their transpose
   X<-cmd.object$points
   
   # finally d
   # the ith element of d is d_i^2-d_{i,n+1}^2
   # d<-diag(X%*%Xt)-new.dist^2
   d <- rowSums(X*X) - new.dist^2 # efficient version of above
   # finally construct the product
   ret<-1/2*((lambda.inverse * t(X)) %*% d)

   # return the result
   return(ret)
}
