# vectorise seq()
vecseq<-function(a,len){

   d<-dim(a)[1]

   matrix(a[,1]+rep(c(0:(len-1)),rep(d,len))*(a[,2]-a[,1])/(len-1),d,len)

}
