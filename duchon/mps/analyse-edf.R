# what happened in the free votes sim? EDF!
library(ggplot2)

# load the data and calculate a summary
bedf<-c()
for(samp.size in c(200,300,400,500)){
   load(paste("freesim-",samp.size,".RData",sep=""))

   # and the EDFs
   edf<-edf[-1,]

   names(edf)<-c("GCV","ML")

   melted.edf<-melt(edf)
   bedf<-rbind(bedf,cbind(melted.edf,rep(samp.size,dim(melted.edf)[2])))
   rm(edf)

}

names(bedf)<-c("Method","EDF","n")

theme_set(theme_bw())
p<-ggplot(bedf)
p<-p+geom_histogram(aes(EDF,fill=Method))
#p<-p+facet_grid(method~n)#,scale="free_y")
p<-p+facet_wrap(~n,nrow=1)#,scale="free_y")
p<-p+labs(y="Frequency")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
print(p)

ggsave(file="mps-edf.pdf",width=6,height=3)
