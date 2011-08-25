# look at the dimension selection for leukemia

library(ggplot2)
library(mgcv)

mds.score.dim<-c()
mds.score.minn<-c()

for(ltype in c("ALL","TEL")){
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
                              method=rep("GCV.Cp",nrow(gcv.score.cv))))
   
   # get the scores as well as dimensions for the selected dimensions
   
   scores<-c()
   for(i in 1:100){
      tmp<-ml.score.cv$score[ml.score.cv$sim==i]
      if(!all(is.na(tmp)))
         scores<-c(scores,tmp[ml.best.dim[i]-1])
   }
   for(i in 1:100){
      tmp<-gcv.score.cv$score[gcv.score.cv$sim==i]
      if(!all(is.na(tmp)))
         scores<-c(scores,tmp[gcv.best.dim[i]-1])
   }
 
   mds.score.min<-rbind(cbind(dim=ml.best.dim,
                              sim=1:length(ml.best.dim)),
                        cbind(dim=gcv.best.dim,
                              sim=1:length(gcv.best.dim)))

   mds.score.min<-as.data.frame(mds.score.min)
   mds.score.min<-cbind(mds.score.min,
                       type=rep(ltype,length(ml.best.dim)+length(gcv.best.dim)),
                       method=c(rep("ML",length(ml.best.dim)),
                                rep("GCV.Cp",length(gcv.best.dim))))
   mds.score.min<-cbind(mds.score.min,score=scores)

   mds.score.minn<-rbind(mds.score.minn,mds.score.min)

}

mds.score.min<-mds.score.minn

#pdf(file="breastcancer-dimselect.pdf",height=7,width=7)

## do silly things with viewports to get the plot working...
#plot.rows<-1
#plot.cols<-2
#Layout <- grid.layout(nrow = plot.rows, ncol = plot.cols,
#                      widths = unit(rep(3,plot.rows*plot.cols),"null"),
#                      heights = unit(rep(3,plot.rows*plot.cols), "null"))
#
#subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
#vplayout <- function(...) {
#     grid.newpage()
#     pushViewport(viewport(layout = Layout))
#}
#grid.newpage()
#pushViewport(viewport(layout = Layout))
##### end of viewport sillyness

#gaussian.dim<-mds.score.dim[1:(nrow(mds.score.dim)/2),]
#gaussian.min<-mds.score.min[1:(nrow(mds.score.min)/2),]
#
#quasi.dim<-mds.score.dim[(nrow(mds.score.dim)/2+1):nrow(mds.score.dim),]
#quasi.min<-mds.score.min[(nrow(mds.score.min)/2+1):nrow(mds.score.min),]


library(ggplot2)
p<-ggplot(mds.score.dim)
p<-p+geom_line(aes(x=dim,y=score,group=sim))
#p<-p+facet_grid(type~method)
p<-p+facet_wrap(~method,scales="free")
p<-p+geom_point(aes(x=dim,y=score),size=1.5,colour="red",data=mds.score.min)
p
 

# plot!
#theme_set(theme_bw())
#p<-ggplot(gaussian.dim)
#p<-p+geom_line(aes(x=dim,y=score,group=sim),alpha=0.3)
#p<-p+stat_smooth(aes(x=dim,y=score),alpha=0.3,fill="green",method="gam",formula=y~s(x))
#p<-p+geom_point(aes(x=dim,y=score),size=1.5,colour="red",data=gaussian.min)
#p<-p+labs(x="MDS projection dimension",y="Score")
#p<-p+facet_grid(method~model,scales="free")
#p<-p+opts(panel.grid.major=theme_blank(),
#          panel.grid.minor=theme_blank(),
#          legend.background=theme_blank(),
#          legend.key=theme_blank(),
#          panel.background=theme_rect())
#print(p,vp=subplot(1,1))
#
#p<-ggplot(quasi.dim)
#p<-p+geom_line(aes(x=dim,y=score,group=sim),alpha=0.3)
#p<-p+stat_smooth(aes(x=dim,y=score),alpha=0.3,fill="green",method="gam",formula=y~s(x))
#p<-p+geom_point(aes(x=dim,y=score),size=1.5,colour="red",data=quasi.min)
#p<-p+labs(x="MDS projection dimension",y="Score")
#p<-p+facet_grid(method~model,scales="free")
#p<-p+opts(panel.grid.major=theme_blank(),
#          panel.grid.minor=theme_blank(),
#          legend.background=theme_blank(),
#          legend.key=theme_blank(),
#          panel.background=theme_rect())
#print(p,vp=subplot(1,2))
#
## can't use ggsave(), using viewport!
#
#
#dev.off()

