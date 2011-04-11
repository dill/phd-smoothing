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

for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   grade.samp<-breast.dat$cancer.grade[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.samp,diag=TRUE,upper=TRUE)

   method<-"ds"
   # fit the model
   b.gcv<-gam.mds.fit(grade.samp,breast.dist,NULL,44,c(2,0.90),
#                  family=quasi(link=power(0.6),variance="constant"))
                  family=quasi(link=power(0.4643001),variance="constant"))
   
   # record the GCV score
   this.score<-cbind(as.data.frame(b.gcv$score),
                     rep(i,length(b.gcv$scores$score)),
                     rep(method,length(b.gcv$scores$score)))
   score.cv<-rbind(score.cv,this.score)
   # record the EDF
   this.edf<-c(sum(b.gcv$gam$edf),i,method)
   edf.cv<-rbind(edf.cv,this.edf)
   best.dim<-c(best.dim,b.gcv$mds.dim)   


   # predict the missing value
   pred.data<-as.data.frame(insert.mds.generic(b.gcv$mds.obj,breast.array[i,],breast.samp))
   names(pred.data)<-names(b.gcv$samp.mds)[-1]
   pp<-predict(b.gcv$gam,pred.data,type="response")

   # record the MSE
   ds.mse.cv<-rbind(ds.mse.cv,cbind(as.data.frame(
                                    (breast.dat$cancer.grade[i]-round(pp,0))^2),
                                    method))

   ########################################################
   ### lasso model
   cvmin.lasso<-cv.glmnet(breast.array[-i,],breast.dat$cancer.grade[-i],family="multinomial")
   b.lasso<-glmnet(breast.array[-i,],breast.dat$cancer.grade[-i],
                   family="multinomial",lambda=cvmin.lasso$lambda.min)
   lasso.mse.cv<-c(lasso.mse.cv,(breast.dat$cancer.grade[i]-
                  apply(predict(b.lasso,as.matrix(t(breast.array[i,])),type="response"),1,which.max))^2)
}

names(score.cv)<-c("gcv","dim","booti")
names(ds.mse.cv)<-c("MSE","method")
ds.mse.cv<-ds.mse.cv$MSE


save.image(paste("grade-cv-",b.gcv$gam$family$family,".RData",sep=""))
#
## MSE plot
plot(1:45,seq(min(lasso.mse.cv,ds.mse.cv),max(lasso.mse.cv,ds.mse.cv),len=45),
     xlab="CV round",ylab="MSE",type="n")
lines(1:45,lasso.mse.cv,col="blue")
points(1:45,lasso.mse.cv,pch=19,col="blue")
points(1:45,ds.mse.cv,pch=19)
lines(1:45,ds.mse.cv,pch=19)


# CV score
cat("lasso=",mean(lasso.mse.cv),"\n")
cat("ds=",mean(ds.mse.cv),"\n")
#
## GCV plot
#p<-ggplot(as.data.frame(score.cv))
#p<-p+geom_line(aes(dim,gcv,group=booti))
#p<-p+stat_smooth(aes(dim,gcv))
#print(p)
#
#
#
