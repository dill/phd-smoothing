# what happened in the free votes sim?
library(ggplot2)


# plotting options
theme_set(theme_bw())

# load the data and calculate a summary
load("freesim.RData")
labbin<-labbin[-del.rows,]
lookup<-lookup[-del.rows,]

#
#samp.size<-300
mp.res<-c()
for(samp.size in c(200,300)){
   misclass<-read.csv(file=paste("freesim-misclass-",samp.size,".csv",sep=""))
   
   model.names<-misclass[,687]
   wrong.mat<-misclass[,-c(1,687)]
   
   # now looking at the errors...
   res<-wrong.mat
   res<-matrix(as.numeric(as.matrix(res)),nrow(res),ncol(res))
   res<-res[,mpid]
   
   #### per MP simulations
   
   ds.mps<-res[seq(1,200*5,5),]
   dsml.mps<-res[seq(2,200*5,5),]
   lasso.mps<-res[seq(3,200*5,5),]
   glmnet.mps<-res[seq(4,200*5,5),]
   glm.mps<-res[seq(5,200*5,5),]
   
   ds.mps<-data.frame(wrong=200-colSums(ds.mps),
                      id=mpid,
                      samp.size=rep(samp.size,676),
                      model=rep("ds",676))
   dsml.mps<-data.frame(wrong=200-colSums(dsml.mps),
                        id=mpid,
                        samp.size=rep(samp.size,676),
                        model=rep("dsml",676))
   lasso.mps<-data.frame(wrong=200-colSums(lasso.mps),
                         id=mpid,
                         samp.size=rep(samp.size,676),
                         model=rep("lasso",676))
   glmnet.mps<-data.frame(wrong=200-colSums(glmnet.mps),
                          id=mpid,
                          samp.size=rep(samp.size,676),
                          model=rep("glmnet",676))
   glm.mps<-data.frame(wrong=200-colSums(glm.mps),
                       id=mpid,
                       samp.size=rep(samp.size,676),
                       model=rep("glm",676))
   
   mp.res<-as.data.frame(rbind(mp.res,ds.mps,dsml.mps,glmnet.mps,lasso.mps,glm.mps))

}

# non-zero
mp.res<-mp.res[mp.res$wrong!=0,]


p<-ggplot(mp.res)
p<-p+geom_boxplot(aes(y=wrong,x=factor(model)))
#p<-p+opts(legend.position="none")
p<-p+facet_wrap(~samp.size,nrow=1)
#p<-p+labs(x="MPs",y="Missclassifications")
print(p)


