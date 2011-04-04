source("getdata.R")

# calculate the distance matrix for the microarray data
breast.dist<-dist(breast.array,diag=TRUE,upper=TRUE)

# fit the model
b.gcv<-gam.mds.fit(breast.dat$npi,breast.dist,NULL,45,c(2,0.85))
summary(b.gcv$gam)

# what does the GCV score look like?
#plot(b.gcv$gcvs$dim, b.gcv$gcvs$gcv,ylab="GCV score",xlab="MDS dimension",type="l")


# low EDF, investigate using p.209 of the Red Book
X.mm<-model.matrix(b.gcv$gam)
X.qr.R<-qr.R(qr(X.mm))

S<-b.gcv$gam$smooth[[1]]$S[[1]] # pull out penalty matrix
S<-cbind(rep(0,nrow(S)),S)
S<-rbind(rep(0,ncol(S)),S)


beta.dash<-X.qr.R%*%b.gcv$gam$coefficients
new.pen<-solve(t(X.qr.R))%*%S%*%solve(X.qr.R)
new.pen<-eigen(new.pen)

beta.dash.dash<-t(new.pen$vectors)%*%beta.dash

# so the new parameters are in beta.dash.dash
# the EDF of the ith parameter is
lambda<-b.gcv$gam$sp
EDF<-1/(1+lambda*new.pen$values)

# okay, so EDF and beta.dash.dash are really small, 
# except for coeffs 40:45



## Gamma model - identity link
b.gcv.gamma<-gam.mds.fit(breast.dat$npi,breast.dist,NULL,45,c(2,0.85),fam=Gamma(link=identity))
summary(b.gcv.gamma$gam)


# quasi? -  messing about gives this as best R^2...
b.gcv.quasi<-gam.mds.fit(breast.dat$npi,breast.dist,NULL,45,c(2,0.85),fam=quasi(link=power(1/3),variance="mu^3"))
summary(b.gcv.quasi$gam)

## resid model
resid.data<-b.gcv.quasi$samp.mds
resid.data$response<-b.gcv.quasi$gam$residuals
b.gcv.resids<-gam(response~s(zw,zx,zy,zz,bs="ds",m=c(2,1.5),k=45),data=resid.data)
summary(b.gcv.resids)



# what does the lasso do?
library(glmnet)

# run the full lasso
b.lasso.full<-glmnet(breast.array,breast.dat$npi)

# CV optimal lasso...
cvmin.lasso<-cv.glmnet(breast.array,breast.dat$npi)
b.lasso<-glmnet(breast.array,breast.dat$npi,lambda=cvmin.lasso$lambda.min)



### comparison plot
ind<-1:45

quasi.err<-fitted(b.gcv.quasi$gam)-breast.dat$npi
lasso.err<-predict(b.lasso,breast.array)-breast.dat$npi

pmax<-max(quasi.err,lasso.err)
pmin<-min(quasi.err,lasso.err)

plot(seq(pmin,pmax,len=length(ind)),ind,pch=20,type="n",xlab="bias",ylab="observation")
points(quasi.err,ind,pch=19,cex=0.5,col="red")
points(lasso.err,ind,pch=19,cex=0.5,col="blue")
abline(v=0,lty=2)

for(i in 1:45){
   lines(c(quasi.err[i],lasso.err[i]),c(i,i))
}


### MSE

ds.mse<-sum((breast.dat$npi-fitted(b.gcv.quasi$gam))^2)
lasso.mse<-sum((breast.dat$npi-predict(b.lasso,breast.array))^2)




