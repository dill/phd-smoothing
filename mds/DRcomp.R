# compare D with the Euclidean distance in MDS space...

DRcomp<-function(D,k){

   # take D and do the k-dimensional MDS projection
   mds<-cmdscale(D,eig=TRUE,k=k,x.ret=TRUE)

   # find the distances in MDS space
   X<-mds$points
   
   De<-matrix(NA,dim(D)[1],dim(D)[2])

#   for(i in 1:dim(X)[1]){   
#      for(j in 1:dim(X)[1]){   
#         tmp<-(X[i,1]-X[j,1])^2 + (X[i,2]-X[j,2])^2)
#         De[i,j]<-sqrt((X[i,1]-X[j,1])^2 + (X[i,2]-X[j,2])^2)
#      }
#   }

   De<-as.matrix(dist(X,upper=T,diag=T))

# this was calculating the Frobenius norm, which was the wrong thing!
#   RMSE<-mean((D-De)^2)
#   RMSE

   # want to calculate the spectral norm, 1st eigenvalue of D-De

   eigen(D-De)$value[1]

}
