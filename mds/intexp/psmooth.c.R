# thin plate spline with squash
# taken from smooth.r in mgcv

## The constructor for a P-spline basis object with MDS modifications.
smooth.construct.mdsps.smooth.spec<-function(object,data,knots){

   # make the tprs object as usual
   object<-smooth.construct.ps.smooth.spec(object,data,knots)

   ### recreate the S matrix

   # store the old one first
   oldS<-object$S[[1]] 

   k<-dim(object$S[[1]])[1]
   S<-matrix(0,k,k)

   m <- object$p.order

   if(object$term=="x"){

   # auto ks based adjustment
   kp<-object$knots[((m[1]+2)/2+1):(length(object$knots)-(m[1]+2)/2)]
   this.dat<-eval(parse(text=paste("data$",object$term,sep="")))
   dens.est<-kde(this.dat,h=hpi(this.dat),eval.points=kp)
   sq<-dens.est$estimate

   }else{
   sq<-1
   }

   # calculate the penalty
   S<-diag(object$bs.dim);
   if (m[2]){
      for (i in 1:m[2]){
         S <- diff(S)
      }
   }

   # adjust the penalty
   S<-S*sqrt((1/sq)^3)

   S <- t(S)%*%S

   # enforce symmetry
   S <- (S + t(S))/2

   object$S[[1]]<-S

   cat("max diff=",max(abs(oldS-object$S[[1]])),"\n")

   class(object)<-"mdsps.smooth"
   object
}

# prediction matrix method
Predict.matrix.mdsps.smooth<-function(object,data){
   Predict.matrix.pspline.smooth(object,data)
}
