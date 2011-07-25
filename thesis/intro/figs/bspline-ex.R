# show some examples of B-splines

library(mgcv)
library(splines)


par(mfrow=c(1,3),las=1,mgp=c(2,1,0))


for(bs.dim in c(1,2,3)){
   # create some degree 1 basis functions
   bs.basis<-bs(seq(0,1,len=100),knots=seq(0,1,len=10),degree=bs.dim)
   
   # plot them
   
   
   plot(x=seq(0,1,len=100),y=bs.basis[,1],type="l",
         xlab="x",ylab=bquote(B[j]^.(bs.dim)(x)),asp=1)
   
   for(i in 2:ncol(bs.basis)){
      lines(x=seq(0,1,len=100),y=bs.basis[,i],lty=i)
   }
}
