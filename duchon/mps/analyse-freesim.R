# what happened in the free votes sim?
library(ggplot2)


# plotting options
theme_set(theme_bw())
plot.rows<-2
plot.cols<-1
Layout <- grid.layout(nrow = plot.rows, ncol = plot.cols,
                      widths = unit(rep(3,plot.rows*plot.cols),"null"),
                      heights = unit(rep(3,plot.rows*plot.cols), "null"))

subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
vplayout <- function(...) {
     grid.newpage()
     pushViewport(viewport(layout = Layout))
}
grid.newpage()
pushViewport(viewport(layout = Layout))


# load the data and calculate a summary
bbrier<-c()
bedf<-c()
bmse<-c()
bwrong.mat<-c()

for(samp.size in c(200,300,400,500)){
   load(paste("freesim-",samp.size,".RData",sep=""))

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
print(p,vp=subplot(1,1))

# plot the MSE scores in boxplots
p<-ggplot(bmse)
p<-p+geom_boxplot(aes(method,score))
p<-p+facet_wrap(~samp.size)
print(p,vp=subplot(2,1))



#labbin<-labbin[-del.rows,]
#lookup<-lookup[-del.rows,]
#
##
#samp.size<-300
#misclass<-read.csv(file=paste("freesim-misclass-",samp.size,".csv",sep=""))
#
#model.names<-misclass[,687]
#wrong.mat<-misclass[,-c(1,687)]
#
## now looking at the errors...
#res<-wrong.mat
#res<-matrix(as.numeric(as.matrix(res)),nrow(res),ncol(res))
#res<-res[,mpid]
#
##### per simulation results
#
#sim.res<-data.frame(mse=685-rowSums(res),
#                    model=model.names,
#                    sim=as.numeric(sapply(1:200,rep,5)))
#
#p<-ggplot(sim.res)
#p<-p+geom_histogram(aes(y=mse,sim,binwidth=1),stat="identity")
#p<-p+facet_wrap(~model,nrow=1)
#p<-p+labs(x="Simulation",y="Missclassifications")
#print(p,vp=subplot(1,1))
#
#
##### per MP simulations
#
#ds.mps<-res[seq(1,200*5,5),]
#dsml.mps<-res[seq(2,200*5,5),]
#lasso.mps<-res[seq(3,200*5,5),]
#glmnet.mps<-res[seq(4,200*5,5),]
#glm.mps<-res[seq(5,200*5,5),]
#
#ds.mps<-data.frame(wrong=200-colSums(ds.mps),id=mpid,party=mpparty,model=rep("ds",676))
#dsml.mps<-data.frame(wrong=200-colSums(dsml.mps),id=mpid,party=mpparty,model=rep("dsml",676))
#lasso.mps<-data.frame(wrong=200-colSums(lasso.mps),id=mpid,party=mpparty,model=rep("lasso",676))
#glmnet.mps<-data.frame(wrong=200-colSums(glmnet.mps),id=mpid,party=mpparty,model=rep("glmnet",676))
#glm.mps<-data.frame(wrong=200-colSums(glm.mps),id=mpid,party=mpparty,model=rep("glm",676))
#
#mp.res<-as.data.frame(rbind(ds.mps,dsml.mps,glmnet.mps,lasso.mps,glm.mps))
#
#p<-ggplot(mp.res)
#p<-p+geom_histogram(aes(y=wrong,id,colour=party),stat="identity")
#p<-p+opts(legend.position="none")
#p<-p+facet_wrap(~model,nrow=1)
#p<-p+labs(x="MPs",y="Missclassifications")
#print(p,vp=subplot(2,1))
#
#for(mod in unique(model.names)){
#   cat(mod,mean(sim.res$mse[sim.res$model==mod]),"\n")
#}

