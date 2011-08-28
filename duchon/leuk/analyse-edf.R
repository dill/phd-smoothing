# analyse EDFs for sim and confsim
library(ggplot2)

res<-c()

for(simt in c("","conf-")){
   for(ltype in c("ALL","TEL")){

      if(simt==""){
         simlab<-" 40 genes"
      }else{
         simlab<-"140 genes"
      }

      load(paste(simt,"simtest-",ltype,".RData",sep=""))
      
      if(ltype=="ALL") ltype<-"T-ALL"
      if(ltype=="TEL") ltype<-"TEL-AML1"
   
      res<-rbind(res,cbind(ml.edf.cv,
                           rep(simlab,length(ml.edf.cv)),
                           rep(ltype,length(ml.edf.cv)),
                           rep("ML",length(ml.edf.cv))))
      res<-rbind(res,cbind(gcv.edf.cv,
                           rep(simlab,length(gcv.edf.cv)),
                           rep(ltype,length(gcv.edf.cv)),
                           rep("GCV",length(gcv.edf.cv))))
   }
}

res<-data.frame(Score=as.numeric(res[,1]),
                type=res[,2],
                sim=res[,3],
                Method=res[,4])

theme_set(theme_bw())
p<-ggplot(res)
p<-p+geom_histogram(aes(x=Score,fill=Method))
p<-p+facet_grid(type~sim,scales="free_y")
p<-p+labs(x="EDF",y="Frequency")

p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
print(p)

ggsave("sim-edf.pdf",height=7,width=7)

