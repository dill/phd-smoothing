## plot something
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


p<-ggplot(gcv.score)
p<-p+geom_boxplot(aes(factor(dim),score))
p<-p+geom_smooth(aes(x=dim-2,y=score))
p


