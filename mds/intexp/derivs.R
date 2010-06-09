# derivatives for tprs
# assume here that the penalty order is 2! (ie, m=2)

# just eta in 1D
eta1<-function(x,xk){
   gamma(-1.5)/(16*sqrt(pi))*sqrt((x-xk)^3)
}

# first derivative
deta1<-function(x,xk){
   3*gamma(-1.5)/(16*sqrt(pi))*(1-xk)*(x-xk)^2
}

# second derivative
ddeta1<-function(x,xk){
   6*gamma(-1.5)/(16*sqrt(pi))*(1-xk)^2*(x-xk)
}

int_ddeta1<-function(xk){

   res<-rep(0,length(xk))

   for(i in 1:length(xk)){
      res[i]<-integrate(ddeta1^2,xk=xk[i],lower=-1e16,upper=1e16)
   }

   return(res)
}


# eta in 2D
deta2x1<-function(x,xk){
   ss<-sum((x-xk)^2)
   (1/(8*pi))*ss*log(sqrt(ss))
}

# 1st deriv eta in 2D, x1
deta2x1<-function(x,xk){
   ss<-sum((x-xk)^2)
   (1/(8*pi))*(x1-xk[1])*(1+log(ss))
}

# 1st deriv eta in 2D, x2
deta2x2<-function(x,xk){
   ss<-sum((x-xk)^2)
   (1/(8*pi))*(x2-xk[2])*(1+log(ss))
}

# second derivative x1^2
ddeta2x1<-function(x1,x2,xk){
   ss<-sum((x-xk)^2)
   (1/(8*pi))*(1 + log(sqrt(ss)) + 2*((x1-xk[1])^2)/(ss))
}

# second derivative x2^2
ddeta2x1<-function(x1,x2,xk){
   ss<-sum((x-xk)^2)
   (1/(8*pi))*(1 + log(sqrt(ss)) + 2*((x2-xk[2])^2)/(ss))
}




