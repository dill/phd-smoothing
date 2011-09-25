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

   method.names<-c("MDS+DS\n(GCV)","MDS+DS\n(ML)","lasso","glm")
   ### BUG
   # there was an extra column in the data.frames used to hold the results
   # to get around this:
   brier<-brier[,-5]
   names(brier)<-method.names
   mse<-mse[,-5]
   names(mse)<-method.names

   # make a frame of Brier scores
   brier<-brier[-1,]
   cols<-c()

   # do colour coding for boxplot
   testi<-3 # test against lasso
   for(i in 1:4){
      if(i!=testi){
         pv<-wilcox.test(brier[,testi],brier[,i],paired=T)$p.value
         med<-median(brier[,i]-brier[,testi])

         if(pv<0.01 & med>0){
            cols<-c(cols,rep("red",nrow(brier)))
         }else if(pv<0.01 & med<0){
            cols<-c(cols,rep("green",nrow(brier)))
         }else{
            cols<-c(cols,rep("white",nrow(brier)))
         }

      }else{
         cols<-c(cols,rep("white",nrow(brier)))
      }
   }

   melted.brier<-melt(brier)
   bbrier<-rbind(bbrier,cbind(melted.brier,
                              rep(samp.size,dim(melted.brier)[2]),
                              cols))
#   rm(brier)

   # and for MSE
   mse<-mse[-1,]
   cols<-c()

   # do colour coding for boxplot
   testi<-3 # test against lasso
   for(i in 1:4){
      if(i!=testi){
         pv<-wilcox.test(mse[,testi],mse[,i],paired=T)$p.value
         med<-median(mse[,i]-mse[,testi])

         if(pv<0.01 & med>0){
            cols<-c(cols,rep("red",nrow(mse)))
         }else if(pv<0.01 & med<0){
            cols<-c(cols,rep("green",nrow(mse)))
         }else{
            cols<-c(cols,rep("white",nrow(mse)))
         }

      }else{
         cols<-c(cols,rep("white",nrow(brier)))
      }
   }
   melted.mse<-melt(mse)
   bmse<-rbind(bmse,cbind(melted.mse,
                          rep(samp.size,dim(melted.mse)[2]),
                          cols))
#   rm(mse)

}

# all significantly different from lasso!!


# add some column names
names(bbrier)<-c("method","score","samp.size","cols")
names(bmse)<-c("method","score","samp.size","cols")

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

