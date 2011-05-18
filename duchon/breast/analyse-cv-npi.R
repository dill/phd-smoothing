# analyse the breast cancer CV simulation
# both Normal and quasi error models

library(ggplot2)
library(mgcv)

# load the Normal error data
load("npi-cv-gaussian.RData")

# put the score vs. dim and min dim into a big data.frame

names(gcv.score.cv)<-names(ml.score.cv)

mds.score.dim<-rbind(cbind(ml.score.cv,
                           model=rep("gaussian",nrow(ml.score.cv)),
                           method=rep("ML",nrow(ml.score.cv))),
                     cbind(gcv.score.cv,
                           model=rep("gaussian",nrow(ml.score.cv)),
                           method=rep("GCV.Cp",nrow(gcv.score.cv))))

# get the scores as well as dimensions for the selected dimensions

scores<-c()
for(i in 1:45){
   tmp<-ml.score.cv$score[ml.score.cv$booti==i]
   scores<-c(scores,tmp[ml.best.dim[i]-1])
}
for(i in 1:45){
   tmp<-gcv.score.cv$score[gcv.score.cv$booti==i]
   scores<-c(scores,tmp[gcv.best.dim[i]-1])
}


mds.score.min<-rbind(cbind(dim=ml.best.dim,
                           sim=1:length(ml.best.dim)),
                     cbind(dim=gcv.best.dim,
                           sim=1:length(gcv.best.dim)))
               
mds.score.min<-as.data.frame(mds.score.min)
mds.score.min<-cbind(mds.score.min,
                     model=rep("gaussian",length(ml.best.dim)+length(gcv.best.dim)),
                     method=c(rep("ML",length(ml.best.dim)),
                              rep("GCV.Cp",length(gcv.best.dim))))
mds.score.min<-cbind(mds.score.min,score=scores)

names(mds.score.dim)[3]<-"sim"

# plot!
p<-ggplot(mds.score.dim)
p<-p+geom_line(aes(x=dim,y=score,group=sim),alpha=0.3)
p<-p+stat_smooth(aes(x=dim,y=score),alpha=0.3,fill="green",method="gam",formula=y~s(x))
p<-p+geom_point(aes(x=dim,y=score),size=1.5,colour="red",data=mds.score.min)
p<-p+labs(x="MDS projection dimension",y="Score")
p<-p+facet_grid(method~model,scales="free")
print(p)

### ggsave!
#ggsave("mps-dimselect.pdf")








# MSE plot
#plot(1:45,seq(min(lasso.mse.cv,dsml.mse.cv,dsgcv.mse.cv),
#              max(lasso.mse.cv,dsml.mse.cv,dsgcv.mse.cv),len=45),
#     xlab="CV round",ylab="MSE",type="n")
#lines(1:45,lasso.mse.cv,col="blue")
#points(1:45,lasso.mse.cv,pch=19,col="blue")
#points(1:45,dsml.mse.cv,pch=19)
#lines(1:45,dsml.mse.cv,pch=19)
#points(1:45,dsgcv.mse.cv,pch=19,col="red")
#lines(1:45,dsgcv.mse.cv,pch=19,col="red")
#
## CV score
#cat("lasso=",mean(lasso.mse.cv),"\n")
#cat("ds ML=",mean(dsml.mse.cv),"\n")
#cat("ds GCV=",mean(dsgcv.mse.cv),"\n")
#
