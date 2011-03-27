source("getdata.R")

library(glmnet)

# how sensitive are the GCV results to the data?
# using leave-one-out cross validation

# general setup
b.rows<-2#nrow(breast.array)
gcv.cv<-c()
ds.mse.cv<-c()
lasso.mse.cv<-c()

for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   grade.samp<-breast.dat$cancer.grade[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.samp,diag=TRUE,upper=TRUE)

   # fit the model
   b.gcv<-gam.mds.fit(grade.samp,breast.dist,NULL,40,
                  c(2,0.85),fam=quasi(link=power(0.4643001),variance="constant"))

   # record the GCV
   this.gcv<-cbind(as.data.frame(b.gcv$gcvs),rep(i,length(b.gcv$gcvs$gcv)))
   gcv.cv<-rbind(gcv.cv,this.gcv)

   # do some prediction
   pred.data<-as.data.frame(insert.mds.generic(b.gcv$mds.obj,breast.array[i,],breast.samp))
   names(pred.data)<-names(b.gcv$samp.mds)[-1]
   pp<-predict(b.gcv$gam,pred.data)
   model.preds<-rep(NA,length(breast.dat$npi))
   model.preds[-i]<-fitted(b.gcv$gam)
   model.preds[i]<-pp
   # record the MSE
   ds.mse.cv<-c(ds.mse.cv,sum((breast.dat$cancer.grade-round(model.preds,0))^2))

   ### lasso model
   cvmin.lasso<-cv.glmnet(breast.array[-i,],breast.dat$cancer.grade[-i],family="multinomial")
   b.lasso<-glmnet(breast.array[-i,],breast.dat$cancer.grade[-i],
                   family="multinomial",lambda=cvmin.lasso$lambda.min)
   lasso.mse.cv<-c(lasso.mse.cv,sum((breast.dat$cancer.grade-
                  apply(predict(b.lasso,breast.array,type="response"),1,which.max))^2))
}

names(gcv.cv)<-c("gcv","dim","booti")

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
p<-ggplot(as.data.frame(gcv.cv))
p<-p+geom_line(aes(dim,gcv,group=booti))


## data mudging
#gcv.mins<-data.frame(gcv=apply(gcv.boot,1,min),dim=apply(gcv.boot,1,which.min))
#
#gcv.boot<-melt(gcv.boot)
#names(gcv.boot)<-c("sim","dim","gcv")
#
## plotting
#p<-ggplot(gcv.boot)
#p<-p+geom_line(aes(x=dim+2,y=gcv,group=sim),alpha=0.3)
#p<-p+geom_smooth(aes(x=dim+2,y=gcv))
#p<-p+geom_point(aes(x=dim+2,y=gcv),data=gcv.mins,colour="red")
#print(p)
#
### what does the GCV score look like?
##plot(b.gcv$gcvs$dim, b.gcv$gcvs$gcv,ylab="GCV score",xlab="MDS dimension",type="l")
