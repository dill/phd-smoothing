# wrapper for Wang and Ranalli-style thin plate splines
# using the distances from wood.c

# source in some files
source("mds.R")
source("tps.R")


wr<-function(samp,knots,bnd,lambda){

   # expect samp to be a list with elements x,y,z 
   # where z is response
   
   # need to find the distance matrix
   D<-create_distance_matrix(c(samp$x,knots$x),
                             c(samp$y,knots$y),bnd)

   nsamp<-length(samp$x)

   # distances from data to knots
   D.xxk<-D[1:nsamp,(nsamp+1):dim(D)[2]]
   # distances between knots
   D.xkxk<-D[(nsamp+1):dim(D)[2],(nsamp+1):dim(D)[2]]
   
   # find the beta
   beta<-fit.tps(samp$z,matrix(c(samp$x,samp$y),nsamp,2),
                  matrix(c(knots$x,knots$y),length(knots$x),2),
                  lambda=lambda,D.xkxk=D.xkxk,D.xxk=D.xxk)

   # return the parameters
   return(beta)
}

wr.pred<-function(pred,knots,beta){
   # expect pred to be a list with elements x,y,z 
   # where z is response

   # beta just the return from wr

   npred<-length(pred$x)

   # for the prediction points
   D.p<-create_distance_matrix(c(pred$x,knots$x),
                               c(pred$y,knots$y),bnd,
                               start=length(pred$x))

   # distances from prediction points to knots
   #D.xpxk<-D.p[1:npred,(npred+1):dim(D.p)[2]]
   D.xpxk<-D.p

   res<-eval.tps(matrix(c(pred$x,pred$y),npred,2),beta,
                 matrix(c(knots$x,knots$y),length(knots$x),2),D.xpxk=D.xpxk)

   return(res)
}
