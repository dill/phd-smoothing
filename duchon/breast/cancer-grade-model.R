source("getdata.R")

# calculate the distance matrix for the microarray data
breast.dist<-dist(breast.array,diag=TRUE,upper=TRUE)

# fit the model
b.gcv.quasi<-gam.mds.fit(breast.dat$cancer.grade,breast.dist,NULL,45,c(2,0.85),fam=quasi(link=identity,variance="constant"))

e<-b.gcv.quasi$gam$residuals
fv<-fitted(b.gcv.quasi$gam)
(lm(log(e^2)~log(fv))$coeff[2])^2

# indicates that 1/2 looks good, well 0.4634001 ...
b.gcv.quasi<-gam.mds.fit(breast.dat$cancer.grade,breast.dist,NULL,45,c(2,0.85),fam=quasi(link=power(0.4643001),variance="constant"))

# BOOM
summary(b.gcv.quasi$gam)
gam.check(b.gcv.quasi$gam)

# what does the GCV score look like?
plot(b.gcv.quasi$gcvs$dim, b.gcv.quasi$gcvs$gcv,ylab="GCV score",xlab="MDS dimension",type="l")


## resid model
resid.data<-b.gcv.quasi$samp.mds
resid.data$response<-b.gcv.quasi$gam$residuals
b.gcv.resids<-gam(response~s(bw,bx,by,bz,bs="ds",m=c(2,1.5),k=45),data=resid.data)
summary(b.gcv.resids)


# what does the lasso do?
library(glmnet)

# run the full lasso
b.lasso.full<-glmnet(breast.array,breast.dat$cancer.grade,family="multinomial")

# CV optimal lasso...
cvmin.lasso<-cv.glmnet(breast.array,breast.dat$cancer.grade,family="multinomial")
b.lasso<-glmnet(breast.array,breast.dat$cancer.grade,family="multinomial",lambda=cvmin.lasso$lambda.min)


### MSE
ds.mse<-sum((breast.dat$cancer.grade-round(fitted(b.gcv.quasi$gam),0))^2)
lasso.mse<-sum((breast.dat$cancer.grade-apply(predict(b.lasso,breast.array,type="response"),1,which.max))^2)

