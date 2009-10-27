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

# functiontofitpenalizedregressionsplinetox,ydata, 
# withknotsxk,givensmoothingparameter,lambda. 
prs.fit<-function(y,x,xk,lambda) {
   q<-length(xk)+2 #dimensionofbasis 
   n<-length(x) #numberofdata 
   # createaugmentedmodelmatrix.... 
   Xa<-rbind(spl.X(x,xk),mat.sqrt(spl.S(xk))*sqrt(lambda)) 
   y[(n+1):(n+q)]<-0 #augmentthedatavector 
   lm(y~Xa-1)#fitandreturnpenalizedregressionspline 
} 

xp<-seq(0,1,len=120) # xvaluesforprediction 
xk<-1:7/8 #choosesomeknots 
#xk<-c(unique(x),1.3)
mod.2<-prs.fit(wear,x,xk,0.0001)# fitpen.reg.spline 
Xp<-spl.X(xp,xk)#matrixtomapparamstofittedvaluesatxp 

par(mfrow=c(3,1))
#plot data & spl.fit 
plot(x,wear, main="normal fit")
lines(xp,Xp%*%coef(mod.2))
abline(v=xk,col="green",lwd=2)
rug(x,lwd=2)



# now crazy things happen
# move around the values of x and xk
xk<-1:7/8 #choose some knots 
xp<-seq(0,1.2,len=120) # xvaluesforprediction 


x[x>xk[3]]<-x[x>xk[3]]+0.2
xp.m<-xp
xp.m[xp.m>xk[3]]<-xp.m[xp.m>xk[3]]+0.2
xk[xk>xk[3]]<-xk[xk>xk[3]]+0.2
x[x>xk[4] & x<xk[7]]<-x[x>xk[4] & x<xk[7]]-0.1
xp.m[xp.m>xk[4] & xp.m<xk[7]]<-xp.m[xp.m>xk[4] & xp.m<xk[7]]-0.1
xk[xk>xk[4] & xk<xk[7]]<-xk[xk>xk[4] & xk<xk[7]]-0.2


mod.2<-prs.fit(wear,x,xk,0.0001)# fitpen.reg.spline 
Xp.move<-spl.X(xp.m,xk)#matrix to map params to fittedv alues at xp 


#plot data & spl.fit 
plot(x,wear, main="squash fit")
lines(xp,Xp.move%*%coef(mod.2))
abline(v=xk,col="green",lwd=2)
rug(x,lwd=2)


# fixing...


spl.S<-function(xk){ 
   # set up the penalized regression spline penalty matrix, 
   # given knot sequence xk 
   q<-length(xk)+2;S<-matrix(0,q,q) # initialize matrix to 0 
   S[3:q,3:q]<-outer(xk,xk,FUN=rk) # fill in non-zero part 
   S 
} 

# migth be differentials of R
Rd<-function(x,z){1/2*((z-1/2)^2-1/2)-1/3*(abs(x-z)-1/2)+1/24}
Rd<-function(x,z){1/12*(6*(z^2-z-abs(z-x)+z(-x)^2)+2)}
Rdd<-function(x,xk1,xk2){Rd(x,xk1)*Rd(x,xk2)}
integrate(Rdd,lower=0,upper=1,xk1=xk[1],xk2=xk[2])
rk(xk[1],xk[2])





