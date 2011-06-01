# what happened in the free votes sim?
# plot: brier and mse score per model 
#       2x2 lattice for sample size
library(ggplot2)


# plotting options
theme_set(theme_bw())
#plot.rows<-2
#plot.cols<-1
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


# load the data and calculate a summary
bbrier<-c()
bedf<-c()
bmse<-c()
bwrong.mat<-c()

for(samp.size in c(200,300,400,500)){
   load(paste("freesim-",samp.size,".RData",sep=""))

   method.names<-c("dsgcv","dsml","glmnet","glm")
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

   # and the EDFs
   edf<-edf[-1,]
   melted.edf<-melt(edf)
   bedf<-rbind(bedf,cbind(melted.edf,rep(samp.size,dim(melted.edf)[2])))
   rm(edf)

#   # wrong.mat...
#   # this makes one matrix per method and then sums over the sims to get
#   # the number of times each MP was misclassified
#   for(i in 1:4){
#      split.mat<-wrong.mat[seq(i,nrow(wrong.mat),length(method.names)),]
#      per.mp<-colSums(split.mat,na.rm=T)
#      bwrong.mat<-rbind(bwrong.mat,c(per.mp,samp.size,method.names[i]))
#   }

}


# add some column names
names(bbrier)<-c("method","score","samp.size")
names(bmse)<-c("method","score","samp.size")

# plot the Brier scores in boxplots
p<-ggplot(bbrier)
p<-p+geom_boxplot(aes(method,score))
p<-p+facet_wrap(~samp.size)
p<-p+labs(y="Brier score")
#print(p,vp=subplot(1,1))
print(p)

# save
ggsave("mp-brier.pdf")

# plot the MSE scores in boxplots
p<-ggplot(bmse)
p<-p+geom_boxplot(aes(method,score))
p<-p+facet_wrap(~samp.size)
p<-p+labs(y="MSE")
#print(p,vp=subplot(2,1))
print(p)
ggsave("mp-mse.pdf")

# now plot the EDFs for GCV and ML
#quartz()
#par(mfrow=c(1,2))
#hist(edf[,1],main="",xlab="EDF") 
#hist(edf[,2],main="",xlab="EDF") 

# mudge wrong.mat
#bwrong.mat<-as.data.frame(bwrong.mat)
#bwrong.mat<-melt(bwrong.mat,(ncol(bwrong.mat)-1):ncol(bwrong.mat))
#names(bwrong.mat)<-c("samp.size","method","ignore","score")
#quartz()
#p<-ggplot(bwrong.mat)
#p<-p+geom_histogram(aes(score))
#p<-p+facet_grid(samp.size~method)
#print(p)

