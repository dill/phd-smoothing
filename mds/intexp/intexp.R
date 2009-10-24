# integration experiment.

# uses the functions from section 3.2.1 of the Red Book


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



size<-c(1.42,1.58,1.78,1.99,1.99,1.99,2.13,2.13,2.13, 
2.32,2.32,2.32,2.32,2.32,2.43,2.43,2.78,2.98,2.98) 
wear<-c(4.0,4.2,2.5,2.6,2.8,2.4,3.2,2.4,2.6,4.8,2.9, 
3.8,3.0,2.7,3.1,3.3,3.0,2.8,1.7) 
x<-size-min(size);x<-x/max(x) 
plot(x,wear,xlab="Scaledenginesize",ylab="Wearindex") 

xk<-1:4/5 #choosesomeknots 
X<-spl.X(x,xk)#generatemodelmatrix 
mod.1<-lm(wear~X-1)# fitmodel 
xp<-0:100/100 # xvaluesforprediction 
Xp<-spl.X(xp,xk)# predictionmatrix 
lines(xp,Xp%*%coef(mod.1))#plotfittedspline 


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

xk<-1:7/8 #choosesomeknots 
mod.2<-prs.fit(wear,x,xk,0.0001)# fitpen.reg.spline 
Xp<-spl.X(xp,xk)#matrixtomapparamstofittedvaluesatxp 
plot(x,wear);lines(xp,Xp%*%coef(mod.2))#plotdata&spl.fit 






