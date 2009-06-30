"insert.mds"<-function(new.dist,cmd.object){
   # args
   #  new.dists      new set of distances from the old points to new
   #                 should take the form of a 1 by n matrix, where there are
   #                 and n original points to measure to.
   #  cmd.object     object returned from cmdscale, with eig=TRUE and 
   #                 x.ret=TRUE.

   # ret

   # want to calculate 1/2 * lambda^(-)1 * X' * d


   # first lambda
   nrow<-dim(cmd.object$points,2)
   lambda<-matrix(0,nrow=nrow,ncol=nrow)
   diag(lambda)<-cmd.object$eig
   
   # find lambda^(-1)
   lambda.inverse<-solve(lambda)

   # now find X and then its transpose
   X<-cmd.object$x.ret
   Xt<-t(X)

   # finally d
   d<-diag(X%*%Xt)-

   










}
