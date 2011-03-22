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
new.pen.D<-diag()

# so the new parameters are in beta.dash.dash
# the EDF of the ith parameter is
lambda<-b.gcv$gam$sp
EDF<-1/(1+lambda*new.pen$values)

# okay, so EDF and beta.dash.dash are really small, 
# except for coeffs 40:45



## Gamma model - identity link
b.gcv.gamma<-gam.mds.fit(breast.dat$npi,breast.dist,NULL,45,c(2,0.85),fam=Gamma(link=identity))
summary(b.gcv.gamma$gam)

## resid model
resid.data<-b.gcv.gamma$samp.mds
resid.data$response<-b.gcv.gamma$gam$residuals
b.gcv.resids<-gam(response~s(bw,bx,by,bz,bs="ds",m=c(2,1.5),k=45),data=resid.data)
summary(b.gcv.resids)


# tweedie - this doesn't really get anywhere...
#b.gcv.tweedie<-gam.mds.fit(breast.dat$npi,breast.dist,NULL,45,c(2,0.85),fam=Tweedie(link=identity,p=1))
#summary(b.gcv.gamma$gam)



## plain old tprs



# what does the lasso do?
library(glmnet)

# run the full lasso
b.lasso.full<-glmnet(breast.array,breast.dat$npi)


cvmin.lasso<-cv.glmnet(breast.array,breast.dat$npi)

b.lasso<-glmnet(breast.array,breast.dat$npi,lambda=cvmin.lasso$lambda.min)


