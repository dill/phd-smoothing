source("getdata.R")

library(glmnet)

# how sensitive are the GCV results to the data?
# using leave-one-out cross validation

# general setup
b.rows<-nrow(breast.array)
score.cv<-c()
ds.mse.cv<-c()
lasso.mse.cv<-c()
edf.cv<-c()
best.dim<-c()
rsq<-c()

set.seed(1)

for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   npi.samp<-breast.dat$npi[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.samp,diag=TRUE,upper=TRUE)

   # fit the model
   #b.gcv<-gam.mds.fit(npi.samp,breast.dist,20,44,NULL,
   #b.gcv<-gam.mds.fit(npi.samp,breast.dist,5,44,NULL)
   b.gcv<-gam.mds.fit(npi.samp,breast.dist,NULL,44,c(2,0.85))
#   b.gcv<-gam.mds.fit(npi.samp,breast.dist,5,44,NULL,
#                      family=quasi(link="identity"))
#                      family=Gamma(link="identity"))
#   b.gcv<-gam.mds.fit(npi.samp,breast.dist,NULL,44,c(2,0.85),
#                      family=quasi(link=power(1/3),variance="mu^3"))

   # record the GCV
   this.score<-cbind(as.data.frame(b.gcv$scores),
                     rep(i,length(b.gcv$scores$score)))
   score.cv<-rbind(score.cv,this.score)
   # record the GCV
   this.edf<-c(sum(b.gcv$gam$edf),i)
   edf.cv<-rbind(edf.cv,this.edf)
   # record the selected MDS dimension
   best.dim<-c(best.dim,b.gcv$mds.dim)
   # record adjusted R^2
   rsq<-c(rsq,summary(b.gcv$gam)$r.sq)   

   # do some prediction
   pred.data<-as.data.frame(insert.mds.generic(b.gcv$mds.obj,breast.array[i,],breast.samp))
   names(pred.data)<-names(b.gcv$samp.mds)[-1]
   pp<-predict(b.gcv$gam,pred.data,type="response")

   # record the MSE
   ds.mse.cv<-c(ds.mse.cv,(breast.dat$npi[i]-pp)^2)

   #################################################################
   ### lasso model
   cvmin.lasso<-cv.glmnet(breast.array[-i,],breast.dat$npi[-i])
   b.lasso<-glmnet(breast.array[-i,],breast.dat$npi[-i],lambda=cvmin.lasso$lambda.min)
   lasso.mse.cv<-c(lasso.mse.cv,(breast.dat$npi[i]-
                                 predict(b.lasso,as.matrix(t(breast.array[i,]))))^2)

}

names(score.cv)<-c("gcv","dim","booti")
#names(ds.mse.cv)<-c("MSE","method")
#ds.mse.cv<-ds.mse.cv$MSE

#save.image(paste("npi-cv-",b.gcv$gam$family$family,".RData",sep=""))


# MSE plot
plot(1:45,seq(min(lasso.mse.cv,ds.mse.cv),max(lasso.mse.cv,ds.mse.cv),len=45),
     xlab="CV round",ylab="MSE",type="n")
lines(1:45,lasso.mse.cv,col="blue")
points(1:45,lasso.mse.cv,pch=19,col="blue")
points(1:45,ds.mse.cv,pch=19)
lines(1:45,ds.mse.cv,pch=19)

# CV score
cat("lasso=",mean(lasso.mse.cv),"\n")
cat("ds=",mean(ds.mse.cv),"\n")

# GCV plot
#p<-ggplot(as.data.frame(score.cv))
#p<-p+geom_line(aes(dim,gcv,group=booti))
#p<-p+stat_smooth(aes(dim,gcv))
#print(p)
