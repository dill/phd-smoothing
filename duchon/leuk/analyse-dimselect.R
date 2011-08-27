# look at the dimension selection for leukemia

library(ggplot2)

mds.score.dim<-c()
mds.score.minn<-c()

for(ltype in c("ALL")){#,"TEL")){
   # load the Normal error data
   load(paste("simtest-",ltype,".RData",sep=""))

   # put the score vs. dim and min dim into a big data.frame

   names(gcv.score.cv)<-names(ml.score.cv)<-c("dim","score","sim")
   
   mds.score.dim<-rbind(mds.score.dim,
                        cbind(ml.score.cv,
                              type=rep(ltype,nrow(ml.score.cv)),
                              method=rep("ML",nrow(ml.score.cv))),
                        cbind(gcv.score.cv,
                              type=rep(ltype,nrow(gcv.score.cv)),
                              method=rep("GCV",nrow(gcv.score.cv))))
   
   # get the scores as well as dimensions for the selected dimensions
   
   if(ltype=="TEL"){

      scores<-c()
      simn1<-c()
      for(i in 1:100){
         tmp<-ml.score.cv$score[ml.score.cv$sim==i]
         if(!all(is.na(tmp))){
            scores<-c(scores,tmp[ml.best.dim[i]-1])
            simn1<-c(simn1,i)
         }
      }
      simn2<-c()
      for(i in 1:100){
         tmp<-gcv.score.cv$score[gcv.score.cv$sim==i]
         if(!all(is.na(tmp))){
            scores<-c(scores,tmp[gcv.best.dim[i]-1])
            simn2<-c(simn2,i)
         }
      }
   }else{

      icount<-1

      mbd<-c()
      gbd<-c()

      scores<-c()
      simn1<-1:100#c()
      for(i in 1:100){
         tmp<-ml.score.cv$score[ml.score.cv$sim==i]
         if(!all(is.na(tmp))){
            scores<-c(scores,tmp[ml.best.dim[icount]-1])
            #simn1<-c(simn1,i)
            icount<-icount+1
            mbd<-c(mbd,ml.best.dim[icount])
         }else{
            mbd<-c(mbd,NA)
            scores<-c(scores,NA)
         }
      }
      icount<-1
      simn2<-1:100#c()
      for(i in 1:100){
         tmp<-gcv.score.cv$score[gcv.score.cv$sim==i]
         if(!all(is.na(tmp))){
            scores<-c(scores,tmp[gcv.best.dim[icount]-1])
            #simn2<-c(simn2,i)
            icount<-icount+1
            gbd<-c(gbd,gcv.best.dim[icount])
         }else{
            gbd<-c(gbd,NA)
            scores<-c(scores,NA)
         }
      }

      ml.best.dim<-mbd
      gcv.best.dim<-gbd

   }
 
   mds.score.min<-rbind(cbind(dim=ml.best.dim,
                              sim=simn1),
                        cbind(dim=gcv.best.dim,
                              sim=simn2))

   mds.score.min<-as.data.frame(mds.score.min)
   mds.score.min<-cbind(mds.score.min,
                       type=rep(ltype,length(ml.best.dim)+length(gcv.best.dim)),
                       method=c(rep("ML",length(ml.best.dim)),
                                rep("GCV",length(gcv.best.dim))))
   mds.score.min<-cbind(mds.score.min,score=scores)

   mds.score.minn<-rbind(mds.score.minn,mds.score.min)

}


mds.score.min<-mds.score.minn

library(ggplot2)
p<-ggplot(mds.score.dim)
p<-p+geom_line(aes(x=dim,y=score,group=sim))
p<-p+facet_wrap(type~method,scales="free")
p<-p+geom_point(aes(x=dim,y=score),size=1.5,colour="red",data=mds.score.min)
print(p)
 

