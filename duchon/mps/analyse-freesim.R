# what happened in the free votes sim?
# plot: brier and mse score per model 
#       2x2 lattice for sample size
library(ggplot2)


# plotting options
theme_set(theme_bw())

# load the data and calculate a summary
bbrier<-c()
bmse<-c()

for(samp.size in c(200,300,400,500)){
   load(paste("freesim-",samp.size,".RData",sep=""))

   method.names<-c("MSG\n(GCV)","MSG\n(ML)","lasso","glm")
   ### BUG
   # there was an extra column in the data.frames used to hold the results
   # to get around this:
   brier<-brier[,-5]
   names(brier)<-method.names
   mse<-mse[,-5]
   names(mse)<-method.names

   # make a frame of Brier scores
   brier<-brier[-1,]

   melted.brier<-melt(brier)
   bbrier<-rbind(bbrier,cbind(melted.brier,rep(samp.size,dim(melted.brier)[2])))
   rm(brier)

   # and for MSE
   mse<-mse[-1,]
   melted.mse<-melt(mse)
   bmse<-rbind(bmse,cbind(melted.mse,rep(samp.size,dim(melted.mse)[2])))
   rm(mse)

}


# add some column names
names(bbrier)<-c("method","score","samp.size")
names(bmse)<-c("method","score","samp.size")

# plot the Brier scores in boxplots
p<-ggplot(bbrier)
p<-p+geom_boxplot(aes(method,score))
p<-p+facet_wrap(~samp.size)
p<-p+labs(y="Brier score")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
print(p)

# save
ggsave("mp-brier.pdf")

# plot the MSE scores in boxplots
p<-ggplot(bmse)
p<-p+geom_boxplot(aes(method,score))
p<-p+facet_wrap(~samp.size)
p<-p+labs(y="MSE")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
print(p)
ggsave("mp-mse.pdf")

