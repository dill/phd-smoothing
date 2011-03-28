source("getdata.R")

library(glmnet)

# how sensitive are the GCV results to the data?
# using leave-one-out cross validation

# general setup
b.rows<-nrow(breast.array)
score.cv<-c()
ds.mse.cv<-c()
lasso.mse.cv<-c()

for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   npi.samp<-breast.dat$npi[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.samp,diag=TRUE,upper=TRUE)

   # use moth ML and GCV scoring
   for(method in c("ML","GCV.Cp")){

      # fit the model
      b.gcv<-gam.mds.fit(npi.samp,breast.dist,NULL,44,c(2,0.85),
                         family=quasi(link=power(1/3),variance="mu^3"),method=method)
   
      # record the GCV
      this.score<-cbind(as.data.frame(b.gcv$scores),
                        rep(i,length(b.gcv$scores$score)),
                        rep(method,length(b.gcv$scores$score)))
      score.cv<-rbind(score.cv,this.score)
   
      # do some prediction
      pred.data<-as.data.frame(insert.mds.generic(b.gcv$mds.obj,breast.array[i,],breast.samp))
      names(pred.data)<-names(b.gcv$samp.mds)[-1]
      pp<-predict(b.gcv$gam,pred.data)
      model.preds<-rep(NA,length(breast.dat$npi))
      model.preds[-i]<-fitted(b.gcv$gam)
      model.preds[i]<-pp
      # record the MSE
      ds.mse.cv<-rbind(ds.mse.cv,cbind(sum((breast.dat$npi-model.preds)^2),method))

   }


   ### lasso model
   cvmin.lasso<-cv.glmnet(breast.array[-i,],breast.dat$npi[-i])
   b.lasso<-glmnet(breast.array[-i,],breast.dat$npi[-i],lambda=cvmin.lasso$lambda.min)
   lasso.mse.cv<-c(lasso.mse.cv,sum((breast.dat$npi-predict(b.lasso,breast.array))^2))

}

names(gcv.cv)<-c("gcv","dim","booti")

save.image("npi-cv.RData")


## MSE plot
#plot(1:45,seq(min(lasso.mse.cv,ds.mse.cv),max(lasso.mse.cv,ds.mse.cv),len=45),
#     xlab="CV round",ylab="MSE",type="n")
#lines(1:45,lasso.mse.cv,col="blue")
#points(1:45,lasso.mse.cv,pch=19,col="blue")
#points(1:45,ds.mse.cv,pch=19)
#lines(1:45,ds.mse.cv,pch=19)
#
## CV score
#cat("lasso=",mean(lasso.mse.cv),"\n")
#cat("ds=",mean(ds.mse.cv),"\n")
#
## GCV plot
#p<-ggplot(as.data.frame(gcv.cv))
#p<-p+geom_line(aes(dim,gcv,group=booti))
