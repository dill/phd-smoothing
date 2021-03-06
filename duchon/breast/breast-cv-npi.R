source("getdata.R")

library(glmnet)

# how sensitive are the GCV results to the data?
# using leave-one-out cross validation

# general setup
b.rows<-nrow(breast.array)
ml.score.cv<-c()
gcv.score.cv<-c()
dsml.mse.cv<-c()
dsgcv.mse.cv<-c()
lasso.mse.cv<-c()
#edf.cv<-c()
ml.best.dim<-c()
gcv.best.dim<-c()

set.seed(11242)

for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   npi.samp<-breast.dat$npi[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
#   breast.dist<-as.matrix(dist(breast.samp,diag=TRUE,upper=TRUE))
   breast.dist<-apply(breast.samp,1,mahalanobis,x=breast.samp,cov=cov(breast.samp))

   ### for both GCV and ML dimension selection
   ### GCV
   # fit the model
   b.gcv<-gam.mds.fit(npi.samp,breast.dist,NULL,44,c(2,0.85),method="GCV.Cp",
                      dist.metric="mahalanobis",fam=quasi(link=power(0.5),variance="mu^2"))

   #,fam=quasi(link=power(0.5),variance="mu^2")
   # record the GCV
   this.score<-cbind(as.data.frame(b.gcv$scores),
                     rep(i,length(b.gcv$scores$score)))
   gcv.score.cv<-rbind(gcv.score.cv,this.score)
   # record the selected MDS dimension
   gcv.best.dim<-c(gcv.best.dim,b.gcv$mds.dim)

   # do some prediction
   pred.data<-as.data.frame(insert.mds.generic(b.gcv$mds.obj,breast.array[i,],breast.samp),dist.metric="mahalanobis")
   names(pred.data)<-names(b.gcv$samp.mds)[-1]
   pp<-predict(b.gcv$gam,pred.data,type="response")

   # record the MSE
   dsgcv.mse.cv<-c(dsgcv.mse.cv,(breast.dat$npi[i]-pp)^2)

   ### P-ML
   # fit the model
   b.ml<-gam.mds.fit(npi.samp,breast.dist,NULL,44,c(2,0.85),method="P-ML",
                      dist.metric="mahalanobis",quasi(link=power(0.5),variance="mu^2")) 

   # record the score
   this.score<-cbind(as.data.frame(b.ml$scores),
                     rep(i,length(b.ml$scores$score)))
   ml.score.cv<-rbind(ml.score.cv,this.score)
   # record the selected MDS dimension
   ml.best.dim<-c(ml.best.dim,b.ml$mds.dim)

   # do some prediction
   pred.data<-as.data.frame(insert.mds.generic(b.ml$mds.obj,breast.array[i,],breast.samp),dist.metric="mahalanobis")
   names(pred.data)<-names(b.ml$samp.mds)[-1]
   pp<-predict(b.ml$gam,pred.data,type="response")

   # record the MSE
   dsml.mse.cv<-c(dsml.mse.cv,(breast.dat$npi[i]-pp)^2)

   #################################################################
   ### lasso model
   cvmin.lasso<-cv.glmnet(breast.array[-i,],breast.dat$npi[-i])
   b.lasso<-glmnet(breast.array[-i,],breast.dat$npi[-i],lambda=cvmin.lasso$lambda.min)
   lasso.mse.cv<-c(lasso.mse.cv,(breast.dat$npi[i]-
                                 predict(b.lasso,as.matrix(t(breast.array[i,]))))^2)

}

names(gcv.score.cv)<-c("dim","gcv","booti")
names(ml.score.cv)<-c("dim","score","booti")

save.image(paste("npi-cv-",b.gcv$gam$family$family,".RData",sep=""))

