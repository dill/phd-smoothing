# comparison of using GCV and ML for selecting the number 
# of dimensions to project into

### PLOTTING

# plot something
load("gcvml.RData")
library(dillhandy)
library(ggplot2)

real.results<-as.data.frame(real.results)
real.results[,1]<-as.numeric(as.character(real.results[,1]))
real.results[,2]<-as.numeric(as.character(real.results[,2]))+2
real.results[,4]<-as.numeric(as.character(real.results[,4]))

# because I'm an idiot
real.names<-c("gcv.score","gcv.aic","gcv.mse",
              "ml.score","ml.aic","ml.mse",
              "reml.score","reml.aic","reml.mse")
real.results[,3]<-rep(real.names,nrow(real.results)/9)


ml.mse<-as.data.frame(pe(real.results,real.results[,3]=="ml.mse"))
gcv.mse<-as.data.frame(pe(real.results,real.results[,3]=="gcv.mse"))
ml.score<-as.data.frame(pe(real.results,real.results[,3]=="ml.score"))
gcv.score<-as.data.frame(pe(real.results,real.results[,3]=="gcv.score"))
reml.score<-as.data.frame(pe(real.results,real.results[,3]=="reml.score"))

names(ml.mse)<-names(gcv.mse)<-names(ml.score)<-names(gcv.score)<-c("sim","dim","name","score")

# plot layout
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


theme_set(theme_bw())


grid.newpage()
pushViewport(viewport(layout = Layout))


## GCV - score and MSE
#p<-ggplot(gcv.score)
#p<-p+geom_boxplot(aes(factor(dim),score))
##p<-p+geom_smooth(aes(x=dim-2,y=score))
#p<-p+labs(x="MDS projection dimension",y="GCV score")
#p<-p+opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), panel.background=theme_rect())
#print(p,vp=subplot(1,1))
#
#p<-ggplot(gcv.mse)
#p<-p+geom_boxplot(aes(factor(dim),score))
##p<-p+geom_smooth(aes(x=dim-2,y=score))
#p<-p+labs(x="MDS projection dimension",y="MSE (for GCV)")
#p<-p+opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), panel.background=theme_rect())
#print(p,vp=subplot(1,2))



# (roll your own) AIC
gcv.aic<-as.data.frame(pe(real.results,real.results[,3]=="gcv.aic"))
gcv.aic<-cbind(gcv.aic,rep("gcv",dim(gcv.aic)[1]))


# for ML and REML, take the ML/REML score (negative log marginal likelihood 
# or negative log restricted likelihood), multiply by -2 then add 2*dimension
ml.aic<-ml.score
reml.aic<-reml.score

ml.aic[,4]<-2*ml.aic[,2]+2*ml.aic[,4]
reml.aic[,4]<-2*reml.aic[,2]+2*reml.aic[,4]

ml.aic<-cbind(ml.aic,rep("ml",dim(ml.aic)[1]))
reml.aic<-cbind(reml.aic,rep("reml",dim(reml.aic)[1]))

names(gcv.aic)<-names(ml.aic)<-names(reml.aic)<-c("sim","dim","name","score","fit")
aics<-rbind(gcv.aic,ml.aic,reml.aic)


### minima
ml.mat<-matrix(ml.aic$score,18,60)
ml.min<-data.frame(score=apply(ml.mat,2,min),dim=apply(ml.mat,2,which.min),fit="ml")
reml.mat<-matrix(reml.aic$score,18,60)
reml.min<-data.frame(score=apply(reml.mat,2,min),dim=apply(reml.mat,2,which.min),fit="reml")
gcv.mat<-matrix(gcv.aic$score,18,60)
gcv.min<-data.frame(score=apply(gcv.mat,2,min),dim=apply(gcv.mat,2,which.min),fit="gcv")

min.mat<-rbind(ml.min,reml.min,gcv.min)
min.mat$dim<-c(3:20)[min.mat$dim]


# boxplots
p<-ggplot(aics)
p<-p+geom_boxplot(aes(factor(dim),score))
p<-p+facet_wrap(~fit,nrow=1)
p<-p+labs(x="MDS projection dimension",y="AIC")
p<-p+opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), panel.background=theme_rect())
print(p,vp=subplot(1,1))



# line plots
p<-ggplot(aics)
p<-p+geom_line(aes(dim,score,group=sim),alpha=0.6)
p<-p+geom_point(aes(x=dim,y=score),data=min.mat,colour="red")
p<-p+facet_wrap(~fit,nrow=1)
p<-p+labs(x="MDS projection dimension",y="AIC")
p<-p+opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), panel.background=theme_rect())
print(p,vp=subplot(2,1))



