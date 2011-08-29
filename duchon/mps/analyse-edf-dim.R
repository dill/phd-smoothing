# what happened in the free votes sim?
library(ggplot2)
library(mgcv)


# plotting options
theme_set(theme_bw())

bigdat<-c()
big.mins<-c()
big.edfs<-c()

for(samp.size in c(200,300,400,500)){
   load(paste("freesim-",samp.size,".RData",sep=""))


   names(mds.dim.sel)<-c("dim","score","method","samp.size","sim")

   min.gcvs<-c()#data.frame(dim=NA,score=NA,sim=NA,method=NA)
   for(method in c("ML","GCV.Cp")){
      for(i in 1:200){
         tmp<-mds.dim.sel[mds.dim.sel$sim==i & mds.dim.sel$method==method,]
         min.gcvs<-rbind(min.gcvs,
                       cbind(tmp$dim[which.min(tmp$score)],
                             min(tmp$score,na.rm=T),
                             i,samp.size))
      }
   }
   min.gcvs<-as.data.frame(min.gcvs)
   min.gcvs<-cbind(min.gcvs,c(rep("ML",200),rep("GCV",200)))
   names(min.gcvs)<-c("dim","score","sim","samp.size","method")

   # put everything into big data frames
   bigdat<-rbind(bigdat,mds.dim.sel)
   big.mins<-rbind(big.mins,min.gcvs)

   big.edfs<-rbind(big.edfs,
                   cbind(min.gcvs,c(edf[,1][-1],edf[,2][-1])))

}

names(bigdat)<-c("dim","score","method","samp.size","sim")
names(big.mins)<-c("dim","score","sim","samp.size","method")

names(big.edfs)[5:6]<-c("Method","EDF")


p<-ggplot(big.edfs)
p<-p+geom_point(aes(x=dim,y=EDF,colour=Method))
p<-p+facet_grid(.~samp.size)

p<-p+labs(x="Dimension")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
p<-p+geom_abline(slope=1)
print(p)

### ggsave!
ggsave("mps-dim-edf.pdf",width=6,height=3)



