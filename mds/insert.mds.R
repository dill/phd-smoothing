"insert.mds"<-function(new.points,old.points,cmd.object,bnd){
   # insert a new point into the MDS, see Gower (Biometrika, 1968.)
   # args
   #  new.points     points to insert into the MDS
   #  old.points     old MDS points
   #  cmd.object     object returned from cmdscale, with eig=TRUE
   #  bnd            boundary
   # ret
   # coordinates of new points in the mds?
   
   # want to calculate 1/2 * lambda^(-1) * X' * d

   # find lambda^(-1)
   lambda.inverse<-1/cmd.object$eig

   # take the original MDS coordinates and their transpose
   X<-cmd.object$points
   
   # finally d

   # new set of distances from the old points to new
   # should take the form of a n by m matrix, where there are
   # and n original points to measure to and m new points.

   new.dist<-matrix(0,length(old.points$x),length(new.points$x))

#   for(i in 1:length(old.points$x)){
#      for(j in 1:length(new.points$x)){
#         x<-c(old.points$x[i],new.points$x[j])
#         y<-c(old.points$y[i],new.points$y[j])
#
#            new.dist[i,j]<-woodpath(x,y,bnd)[1,2]
#
##            if(new.dist[i,j]>10){
##               cat(new.dist[i,j],"\n")
##            }
#
#      }
#   }

   new.dist<-woodpath(c(old.points$x,new.points$x),c(old.points$y,new.points$y),
                      bnd)
   new.dist<-new.dist[1:length(old.points$x),
                      (length(old.points$x)+1):length(c(old.points$x,new.points$x))]



   # the ith element of d is d_i^2-d_{i,n+1}^2
   # d<-diag(X%*%Xt)-new.dist^2
   d <- rowSums(X*X) - new.dist^2 # efficient version of above
   # finally construct the product
   ret<-1/2*((lambda.inverse * t(X)) %*% d)

   # return the transpose of the result
   # ie. mx2 rather than 2xm matrix
   return(t(ret))
}
