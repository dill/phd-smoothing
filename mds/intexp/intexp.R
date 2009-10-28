# integration experiment.

# uses the functions from section 3.2.1 of the Red Book
# data
size<-c(1.42,1.58,1.78,1.99,1.99,1.99,2.13,2.13,2.13, 
2.32,2.32,2.32,2.32,2.32,2.43,2.43,2.78,2.98,2.98) 
wear<-c(4.0,4.2,2.5,2.6,2.8,2.4,3.2,2.4,2.6,4.8,2.9, 
3.8,3.0,2.7,3.1,3.3,3.0,2.8,1.7) 
x<-size-min(size);x<-x/max(x) 

#R(x,z)forcubicsplineon[0,1]
rk<-function(x,z){
   ((z-0.5)^2-1/12)*((x-0.5)^2-1/12)/4- 
      ((abs(x-z)-0.5)^4-(abs(x-z)-0.5)^2/2+7/240)/24 
} 

spl.X<-function(x,xk){
   #set up model matrix for cubic penalized regression spline
   q<-length(xk)+2 #numberofparameters 
   n<-length(x) #numberofdata 
   X<-matrix(1,n,q)#initializedmodelmatrix 
   X[,2]<-x #setsecondcolumntox 
   X[,3:q]<-outer(x,xk,FUN=rk)#andremainingtoR(x,xk) 
   X 
} 

spl.S<-function(xk){ 
   # set up the penalized regression spline penalty matrix, 
   # given knot sequence xk 
   q<-length(xk)+2;S<-matrix(0,q,q) # initialize matrix to 0 
   S[3:q,3:q]<-outer(xk,xk,FUN=rk) # fill in non-zero part 
   S 
} 

# A simple matrix square root 
mat.sqrt<-function(S){
   d<-eigen(S,symmetric=TRUE) 
   rS<-d$vectors%*%diag(d$values^0.5)%*%t(d$vectors) 
} 

# functiont of it penalized regression spline to x,y data, 
# with knots xk,given smoothing parameter, lambda. 
prs.fit<-function(y,x,xk,lambda) {
   q<-length(xk)+2 #dimension of basis 
   n<-length(x) #number of data 
   # create augmented model matrix.... 
   Xa<-rbind(spl.X(x,xk),mat.sqrt(spl.S(xk))*sqrt(lambda)) 
   y[(n+1):(n+q)]<-0 #augment the data vector 
   lm(y~Xa-1)#fit and return penalized regression spline 
} 

xp<-seq(0,1,len=120) # xvaluesforprediction 
xk<-1:7/8 #choosesomeknots 
#xk<-c(unique(x),1.3)
mod.2<-prs.fit(wear,x,xk,0.0001)# fitpen.reg.spline 
Xp<-spl.X(xp,xk)#matrixtomapparamstofittedvaluesatxp 

par(mfrow=c(2,2))
#plot data & spl.fit 
plot(x,wear, main="normal fit")
lines(xp,Xp%*%coef(mod.2))
abline(v=xk,col="green",lwd=2)
rug(x,lwd=2)



#########################################################################


# now crazy things happen
# move around the values of x and xk
xk<-1:7/8 #choose some knots 
xp<-seq(0,1.2,len=120) # xvaluesforprediction 


# don't think that this works since the knots are at the folds
#xk.m<-xk
#x.m[x.m>xk.m[3]]<-x.m[x.m>xk.m[3]]+0.2
#xp.m<-xp
#xp.m[xp.m>xk.m[3]]<-xp.m[xp.m>xk.m[3]]+0.2
#xk.m[xk.m>xk.m[3]]<-xk.m[xk.m>xk.m[3]]+0.2
#x.m[x.m>xk.m[4] & x.m<xk.m[7]]<-x.m[x.m>xk.m[4] & x.m<xk.m[7]]-0.1
#xp.m[xp.m>xk.m[4] & xp.m<xk.m[7]]<-xp.m[xp.m>xk.m[4] & xp.m<xk.m[7]]-0.1
#xk.m[xk.m>xk.m[4] & xk.m<xk.m[7]]<-xk.m[xk.m>xk.m[4] & xk.m<xk.m[7]]-0.2


# simpler
x.m<-x
xp.m<-xp
xk.m<-xk
x.m[x.m>0.3 & x.m<0.7]<-x.m[x.m>0.3 & x.m<0.7]*0.8
xk.m[xk.m>0.3 & xk.m<0.7]<-xk.m[xk.m>0.3 & xk.m<0.7]*0.8
xp.m[xp.m>0.3 & xp.m<0.7]<-xp.m[xp.m>0.3 & xp.m<0.7]*0.8
x.m[x.m>0.7]<-x.m[x.m>0.7]-0.24
xk.m[xk.m>0.7]<-xk.m[xk.m>0.7]-0.24
xp.m[xp.m>0.7]<-xp.m[xp.m>0.7]-0.24

mod.2<-prs.fit(wear,x.m,xk.m,0.0001)# fitpen.reg.spline 
Xp.move<-spl.X(xp.m,xk.m)#matrix to map params to fittedv alues at xp 


#plot data & spl.fit 
plot(x,wear, main="squash fit")
lines(xp,Xp.move%*%coef(mod.2))
abline(v=xk.m,col="green",lwd=2)
rug(x,lwd=2)

S<-spl.S(xk.m)


#### fixing...

# 2nd differential of R
Rd<-function(x,z){1/12*(6*(z^2-z-abs(z-x)+(z-x)^2)+2)}
# product
Rdd<-function(x,xk1,xk2){Rd(x,xk1)*Rd(x,xk2)}
#integrate(Rdd,lower=0,upper=1,xk1=xk[1],xk2=xk[2])
#rk(xk[1],xk[2])

# function to integrate d^2/dx^2 R(x*_i,x*_j)
intR<-function(xk1,xk2,xk,xk.m,max.x){

   intrange<-c(0,xk.m,max.x)

   # find the weights
   w<-abs(diff(intrange)/diff(c(0,1:7/8,1)))^4
   #w<-rep(1,length(intrange))

   #cat("w=",w,"\n")

   # return vector
   ret<-rep(0,length(intrange))

   for(i in 1:(length(intrange)-1)){
      ret[i]<-integrate(Rdd,lower=intrange[i],upper=intrange[i+1],
                        xk1=xk1,xk2=xk2)$value
      ret[i]<-ret[i]*w[i]
   }
   sum(ret)
}


spl.S<-function(xk,xk.m,max.x){ 
   # set up the penalized regression spline penalty matrix, 
   # given knot sequence xk 
   q<-length(xk)+2
   S<-matrix(0,q,q) # initialize matrix to 0 

   T<-matrix(0,q-2,q-2)

   for(i in 1:(q-2)){
      for(j in 1:(q-2)){
         # compute integral of d^2/dx^2 R(x*_i,x*_j)
         T[i,j]<-intR(xk.m[i],xk.m[j],xk,xk.m,max.x)
      }
   }
  
   # by symmetry 
   #T<-T+t(T)-diag(T)

   S[3:q,3:q]<-T # fill in non-zero part 
   S
} 


prs.fit<-function(y,x,xk,xk.m,lambda) {
   q<-length(xk.m)+2 #dimension of basis 
   n<-length(x) #number of data 
   # create augmented model matrix.... 
   Xa<-rbind(spl.X(x,xk.m),mat.sqrt(spl.S(xk,xk.m,max(x)))*sqrt(lambda)) 
   y[(n+1):(n+q)]<-0 #augment the data vector 
   lm(y~Xa-1)#fit and return penalized regression spline 
} 

mod.2<-prs.fit(wear,x.m,xk,xk.m,0.0001)# fitpen.reg.spline 
Xp.move<-spl.X(xp.m,xk.m)#matrix to map params to fittedv alues at xp 

modS<-spl.S(xk.m,xk,max(x.m))

#plot data & spl.fit 
plot(x,wear, main="fixed fit?")
lines(xp,Xp.move%*%coef(mod.2))
abline(v=xk.m,col="green",lwd=2)
rug(x,lwd=2)





mod.2<-prs.fit(wear,x,xk,xk,0.0001)# fitpen.reg.spline 
Xp.move<-spl.X(xp,xk)#matrix to map params to fittedv alues at xp 

#plot data & spl.fit 
plot(x,wear, main="old fit, new S estimation")
lines(xp,Xp.move%*%coef(mod.2))
abline(v=xk,col="green",lwd=2)
rug(x,lwd=2)






### dia
#plot(x,wear)
#abline(v=0.3,col="green",lwd=3)
#abline(v=0.7,col="green",lwd=3)
#points(x[x>0.3 & x<0.7]*0.8,y=wear[x>0.3 & x<0.7],pch=19,col="red")
#abline(v=0.3*0.8,col="red",lwd=3)
#abline(v=0.7*.8,col="red",lwd=3)
#points(x[x>0.7]-.24,y=wear[x>0.7],pch=19,col="red")

