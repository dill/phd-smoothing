source("getdata.R")

library(glmnet)

# how sensitive are the GCV results to the data?
# using leave-one-out cross validation

# general setup
b.rows<-nrow(breast.array)
link.pow<-c()


for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   grade.samp<-breast.dat$cancer.grade[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.samp,diag=TRUE,upper=TRUE)


   # does the link work every time?
   b.gcv.quasi<-gam.mds.fit(grade.samp,breast.dist,NULL,44,c(2,0.85),
                            family=quasi(link=identity,variance="constant"))
   e<-b.gcv.quasi$gam$residuals
   fv<-fitted(b.gcv.quasi$gam)
   link.pow<-rbind(link.pow,(lm(log(e^2)~log(fv))$coeff[2])^2)

}

## MSE plot
#plot(1:45,seq(min(lasso.mse.cv,ds.mse.cv),max(lasso.mse.cv,ds.mse.cv),len=45),
#     xlab="CV round",ylab="MSE",type="n")
#lines(1:45,lasso.mse.cv,col="blue")
#points(1:45,lasso.mse.cv,pch=19,col="blue")
#points(1:45,ds.mse.cv,pch=19)
#lines(1:45,ds.mse.cv,pch=19)
#
#
## CV score
#cat("lasso=",mean(lasso.mse.cv),"\n")
#cat("ds=",mean(ds.mse.cv),"\n")
#
## GCV plot
#p<-ggplot(as.data.frame(score.cv))
#p<-p+geom_line(aes(dim,gcv,group=booti))
#p<-p+stat_smooth(aes(dim,gcv))
#print(p)
#
#
#
