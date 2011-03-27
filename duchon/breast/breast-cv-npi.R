source("getdata.R")

library(glmnet)

# how sensitive are the GCV results to the data?
# using leave-one-out cross validation

# general setup
b.rows<-nrow(breast.array)
gcv.cv<-c()
ds.mse.cv<-c()
lasso.mse.cv<-c()

for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   npi.samp<-breast.dat$npi[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.samp,diag=TRUE,upper=TRUE)

   # fit the model
   b.gcv<-gam.mds.fit(npi.samp,breast.dist,NULL,44,c(2,0.85),
                            fam=quasi(link=power(1/3),variance="mu^3"))

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
   ds.mse.cv<-c(ds.mse.cv,sum((breast.dat$npi-model.preds)^2))


   ### lasso model
   cvmin.lasso<-cv.glmnet(breast.array[-i,],breast.dat$npi[-i])
   b.lasso<-glmnet(breast.array[-i,],breast.dat$npi[-i],lambda=cvmin.lasso$lambda.min)
   lasso.mse.cv<-c(lasso.mse.cv,sum((breast.dat$npi-predict(b.lasso,breast.array))^2))

}

names(gcv.cv)<-c("gcv","dim","booti")

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
