
load("leuk.RData")
library(mdspack)
library(glmnet)

leuk.type<-leuk[,length(leuk)]
leuk<-leuk[,-length(leuk)]

leuk.type<-as.numeric(as.numeric(leuk.type)==9)#10)

# pull out the 40 genes that best classify ALL
# according to chisq
ALL<-read.csv(file="T-ALL-chisq-genes.csv",header=FALSE)
ALL<-as.matrix(as.character(levels(as.factor(ALL[,1]))))
ALL.cols<-as.matrix(apply(ALL,1,grep,x=colnames(leuk)))

# some of those turned up 2 results, fix that
ALL.cols[3,]<-116
ALL.cols[4,]<-263
ALL.cols[6,]<-1271

ALL.cols<-as.integer(ALL.cols)

leuk<-leuk[,ALL.cols]

# general setup
n.sims<-100
ml.score.cv<-c()
gcv.score.cv<-c()
dsml.mse.cv<-c()
dsml.brier.cv<-c()
dsgcv.mse.cv<-c()
dsgcv.brier.cv<-c()
lasso.brier.cv<-c()
lasso.mse.cv<-c()
ml.edf.cv<-c()
gcv.edf.cv<-c()
ml.best.dim<-c()
gcv.best.dim<-c()

set.seed(11242)


# result vector 
# sim #, ML mse, ML brier, ML projection dimension 

for(i in 1:n.sims){

   samp.ind<-sample(1:nrow(leuk),215)

   # do the sampling
   leuk.samp<-leuk[samp.ind,]
   type.samp<-leuk.type[samp.ind]

   ### DS model

   # calculate the distance matrix for the microarray data
   dd.cov.inv<-solve(cov(leuk.samp))
   dd<-apply(leuk.samp,1,mahalanobis,x=leuk.samp,cov=dd.cov.inv,inverted=TRUE)

   #### for both GCV and ML dimension selection
   #### GCV
   b.gcv<-try(gam.mds.fit(type.samp,dd,NULL,k=100,mds.dim.bnds=c(2,.8),
               dist.metric="mahalanobis",fam=binomial,method="GCV.Cp"))

   if(!is.null(b.gcv) & (class(b.gcv)!="try-error")){
      # record the score
      this.score<-cbind(as.data.frame(b.gcv$scores),
                        rep(i,length(b.gcv$scores$score)))
      gcv.score.cv<-rbind(gcv.score.cv,this.score)
      # record the selected MDS dimension
      gcv.best.dim<-c(gcv.best.dim,b.gcv$mds.dim)
      # record the EDF
      gcv.edf.cv<-c(gcv.edf.cv,sum(b.gcv$gam$edf))

      # do some prediction
      pred.data<-as.data.frame(insert.mds.generic(b.gcv$mds.obj,
                                 leuk[-samp.ind,],
                                 leuk.samp,dist.metric="mahalanobis"))
      names(pred.data)<-names(b.gcv$samp.mds)[-1]
      pp<-predict(b.gcv$gam,pred.data,type="response")

      # record the MSE
      dsgcv.mse.cv<-c(dsgcv.mse.cv,sum((leuk.type[-samp.ind]-round(pp))^2))

      # record the brier score
      dsgcv.brier.cv<-c(dsgcv.brier.cv,sum((leuk.type[-samp.ind]-pp)^2))

      ### P-ML
      # fit the model
      b.ml<-try(gam.mds.fit(type.samp,dd,NULL,k=100,mds.dim.bnds=c(2,.8),
                  dist.metric="mahalanobis",fam=binomial,method="ML"))

      if(!is.null(b.ml) & (class(b.ml)!="try-error")){
         # record the score
         this.score<-cbind(as.data.frame(b.ml$scores),
                           rep(i,length(b.ml$scores$score)))
         ml.score.cv<-rbind(ml.score.cv,this.score)
         # record the selected MDS dimension
         ml.best.dim<-c(ml.best.dim,b.ml$mds.dim)
         # and the edf
         ml.edf.cv<-c(ml.edf.cv,sum(b.ml$gam$edf))

         # do some prediction
         pred.data<-as.data.frame(insert.mds.generic(b.ml$mds.obj,
                                    leuk[-samp.ind,],
                                    leuk.samp,dist.metric="mahalanobis"))
         names(pred.data)<-names(b.ml$samp.mds)[-1]
         pp<-predict(b.ml$gam,pred.data,type="response")

         # record the MSE
         dsml.mse.cv<-c(dsml.mse.cv,sum((leuk.type[-samp.ind]-round(pp))^2))

         # record the brier score
         dsml.brier.cv<-c(dsml.brier.cv,sum((leuk.type[-samp.ind]-pp)^2))

         #################################################################
         ### lasso model
         leuk.samp<-as.matrix(leuk.samp)
         cvmin.lasso<-cv.glmnet(leuk.samp,type.samp,family="binomial")
         b.lasso<-glmnet(leuk.samp,type.samp,lambda=cvmin.lasso$lambda.min,family="binomial")

         # predict the classes for the rest of the data
         pp.lasso<-predict(b.lasso,as.matrix(leuk[-samp.ind,]),type="class")
         pp.lasso[pp.lasso==1]<-0
         pp.lasso[pp.lasso==2]<-1

         lasso.mse.cv<-c(lasso.mse.cv,
                         sum((leuk.type[-samp.ind]-pp.lasso)^2))

         # predict the probabilities of being in the classes
         pp.lasso.p<-predict(b.lasso,as.matrix(leuk[-samp.ind,]),type="response")
         lasso.brier.cv<-c(lasso.brier.cv,
                           sum((leuk.type[-samp.ind]-pp.lasso.p)^2))
 

      }
   }
save.image("simtest-ALL.RData")
}



