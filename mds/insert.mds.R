"insert.mds"<-function(new.dist,cmd.object,orig.coords){
   # args
   #  new.dists      new set of distances from the old points to new
   #                 should take the form of a 1 by n matrix, where there are
   #                 and n original points to measure to.
   #  cmd.object     object returned from cmdscale, with eig=TRUE
   #  orig.coords    the coordinates of the original points
   #                 NB must be double centered

   # ret
   # coordinates of new points in the mds?
   


   # want to calculate 1/2 * lambda^(-)1 * X' * d


   # first lambda
   nrow<-dim(cmd.object$points)[2]
   lambda<-matrix(0,nrow=nrow,ncol=nrow)
   diag(lambda)<-cmd.object$eig
   
   # find lambda^(-1)
   lambda.inverse<-solve(lambda)

   # take the original coordinates and their transpose
   X<-orig.coords
   Xt<-t(orig.coords)

   # finally d
   d<-diag(X%*%Xt)-new.dist^2

   # finally construct the product
   ret<-1/2*(lambda.inverse%*%Xt%*%d)

   # return the result
   return(ret)
}
