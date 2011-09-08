# show some examples of B-splines

library(mgcv)
library(splines)


pdf(file="bspline-ex.pdf",width=10.4,height=3.877)

par(mfrow=c(1,3),las=1,mgp=c(2,1,0))

x<-seq(-1,2,len=400)
knots<-seq(-0.5,1.5,len=20)

for(bs.dim in c(1,2,3)){
   # create some degree 1 basis functions
   bs.basis<-bs(x,knots=knots,degree=bs.dim)
   
   # plot them
   
   
   plot(x=x,y=bs.basis[,1],type="l",
         xlim=c(0,1),ylim=c(0,1),
         xlab="x",ylab=bquote(B[j]^.(bs.dim)(x)),asp=1)
   
   for(i in 2:ncol(bs.basis)){
      lines(x=x,y=bs.basis[,i],lty=i)
   }
}

dev.off()
