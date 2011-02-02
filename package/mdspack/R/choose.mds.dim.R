choose.mds.dim<-function(D,lev=0.9){
   # choose the dimension of the MDS projection using
   # eigendecomposition

   # first, grab the eigenvalues
   ee<-eigen(D)$values

   # sort their absolute values
   aa<-sort(abs(ee),dec=T)

   # proportion explained
   pr.exp<-cumsum(aa)/sum(aa)

   # find the number of dimensions needed to explain lev
   # of the variation
   mds.dim<-min(which(pr.exp>=lev))

   return(mds.dim)
}
