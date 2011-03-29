# do a simulation to see how well we can predict the leukemia data... 
load("leuk.RData")

library(mdspack)
library(glmnet)

### Setup the data...
###############################
# re-ordering by recoding...
leuk.type<-leuk[,length(leuk)]
leuk<-leuk[,-length(leuk)]
new.order<-c(5,7,4,8,3,10,9,2,6,1)
leuk.type<-as.numeric(leuk.type)
leuk.old<-leuk.type
for(i in 1:10){
   leuk.type[leuk.old==new.order[i]]<-i
}

bigletters<-as.vector(sapply(letters,paste,letters,sep=""))
bl.len<-length(bigletters)

leuk<-as.data.frame(as.matrix(leuk))

##############################
## Now do some simulatuon stuff

samp.size<-100
n.sims<-1#200

score.res<-c()
retvec<-rep(1,length(leuk.type))
wrong.mat<-c()

for(i in 1:n.sims){

   # make the sample
   samp.ind<-sample(nrow(leuk),samp.size)
   leuk.samp<-leuk[samp.ind,]
   D.samp<-as.matrix(dist(leuk.samp))
   leuk.type.samp<-leuk.type[samp.ind]
   # make the prediction
   leuk.pred<-leuk[-samp.ind,]
   
   gam.obj<-gam.mds.fit(leuk.type.samp,D.samp,NULL,k=100,mds.dim.bnds=c(20,95),family=quasi())
   # record the score
   this.score<-as.data.frame(gam.obj$scores)
   this.score<-cbind(this.score,rep("gcv",nrow(this.score)))
   score.res<-rbind(score.res,this.score)

   # assign some objects
   b<-gam.obj$gam
   mds.dim<-gam.obj$mds.dim
   mds.obj<-gam.obj$mds.obj
   samp.mds<-gam.obj$samp.mds

   samp.mds<-as.data.frame(samp.mds)

   ### predictions
   # map the predictions
   # using code from insert.mds

   pred.mds<-insert.mds.generic(mds.obj,leuk.pred,leuk.samp)

   # predict back over all observations
   pred.grid<-matrix(NA,length(leuk.type),mds.dim)
   pred.grid[samp.ind,]<-as.matrix(samp.mds)[,-1]
   pred.grid[-samp.ind,]<-pred.mds
   pred.grid<-as.data.frame(pred.grid)
   attr(pred.grid,"names")<-bigletters[(length(bigletters)-
                                       (dim(samp.mds)[2]-2)):length(bigletters)]

   pr<-predict(b,pred.grid,type="response")
   pr<-round(pr,0)

   wrongvec<-retvec
   wrongvec[(t(t(pr))-leuk.type)!=0]<-0
   wrong.mat<-rbind(wrong.mat,c(wrongvec,"ds"))

   ##################################################
   # trying glmnet lasso instead?
   cv.lasso<-cv.glmnet(as.matrix(leuk.samp),leuk.type.samp,family="multinomial")
   lasso.obj<-glmnet(as.matrix(leuk.samp),leuk.type.samp,family="multinomial",
                     lambda=cv.lasso$lambda.min)

   pp<-predict(lasso.obj,leuk,type="response")

   pp<-apply(max,2,pp)

   wrongvec<-retvec
   wrongvec[(t(t(pp))-leuk.type)!=0]<-0
   wrong.mat<-rbind(wrong.mat,c(wrongvec,"glmnet"))
   ##################################################
}



