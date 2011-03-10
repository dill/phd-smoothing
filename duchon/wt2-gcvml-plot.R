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

ml.mse<-as.data.frame(pe(real.results,real.results[,3]=="ml.mse"))
gcv.mse<-as.data.frame(pe(real.results,real.results[,3]=="gcv.mse"))
ml.score<-as.data.frame(pe(real.results,real.results[,3]=="ml.score"))
gcv.score<-as.data.frame(pe(real.results,real.results[,3]=="gcv.score"))

names(ml.mse)<-names(gcv.mse)<-names(ml.score)<-names(gcv.score)<-c("sim","dim","name","score")

# setup for a 1x2 plot
Layout <- grid.layout(nrow = 1, ncol = 2, 
                      widths = unit(rep(1,2),"null"), 
                      heights = unit(rep(1,1), "null"))

subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
vplayout <- function(...) {
     grid.newpage()
     pushViewport(viewport(layout = Layout))
}


theme_set(theme_bw())


grid.newpage()
pushViewport(viewport(layout = Layout))

p<-ggplot(gcv.score)
p<-p+geom_boxplot(aes(factor(dim),score))
#p<-p+geom_smooth(aes(x=dim-2,y=score))
p<-p+labs(x="MDS projection dimension",y="GCV score")
p<-p+opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), panel.background=theme_rect())
print(p,vp=subplot(1,1))

p<-ggplot(gcv.mse)
p<-p+geom_boxplot(aes(factor(dim),score))
#p<-p+geom_smooth(aes(x=dim-2,y=score))
p<-p+labs(x="MDS projection dimension",y="MSE")
p<-p+opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), panel.background=theme_rect())
print(p,vp=subplot(1,2))



# AIC
aics<-as.data.frame(pe(real.results,real.results[,3]=="ml.aic"))
gcv.aic<-aics[seq(1,dim(aics)[1],by=3),]
gcv.aic<-cbind(gcv.aic,rep("gcv",dim(gcv.aic)[1]))
ml.aic<-aics[seq(2,dim(aics)[1],by=3),]
ml.aic<-cbind(ml.aic,rep("ml",dim(ml.aic)[1]))
reml.aic<-aics[seq(3,dim(aics)[1],by=3),]
reml.aic<-cbind(reml.aic,rep("reml",dim(reml.aic)[1]))

names(gcv.aic)<-names(ml.aic)<-names(reml.aic)<-c("sim","dim","name","score","fit")

aics<-rbind(gcv.aic,ml.aic,reml.aic)



p<-ggplot(aics)
p<-p+geom_boxplot(aes(factor(dim),score))
p<-p+facet_wrap(~fit,nrow=1)
print(p)




## ML score decreases
#p<-ggplot(ml.score)
#p<-p+geom_boxplot(aes(factor(dim),score))
#p
#p<-p+geom_line(aes(x=dim,y=score,group=sim))
#p<-ggplot(ml.mse)
#p<-p+geom_boxplot(aes(factor(dim),score))
##p<-p+geom_line(aes(x=dim,y=score,group=sim))
#p
## REML score just decreases too!
#reml.score<-real.results[seq(7,dim(real.results)[1],9),]
#names(reml.score)<-c("sim","dim","name","score")
#p<-ggplot(reml.score)
#p<-p+geom_boxplot(aes(factor(dim),score))
#p



## popularity contest
#a<-rep(NA,60)
#for(i in 1:60){
#   a[i]<-gcv.score$dim[gcv.score$sim==i][which.min(gcv.score$score[gcv.score$sim==i])]
#}
#attr(which.max(table(a)),"names")
#
#a<-rep(NA,60)
#for(i in 1:60){
#   a[i]<-ml.score$dim[ml.score$sim==i][which.min(ml.score$score[ml.score$sim==i])]
#}
#attr(which.max(table(a)),"names")
#
#
#a<-rep(NA,60)
#for(i in 1:60){
#   a[i]<-gcv.mse$dim[gcv.mse$sim==i][which.min(gcv.mse$score[gcv.mse$sim==i])]
#}
#attr(which.max(table(a)),"names")
#
#a<-rep(NA,60)
#for(i in 1:60){
#   a[i]<-ml.mse$dim[ml.mse$sim==i][which.min(ml.mse$score[ml.mse$sim==i])]
#}
#attr(which.max(table(a)),"names")


