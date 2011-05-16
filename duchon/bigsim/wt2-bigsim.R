# comparison of using GCV and ML for selecting the number 
# of dimensions to project into

library(mdspack)

#set.seed(123)
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata<-read.csv("wt2truth.csv",header=TRUE)
onoff<-gendata$inside==1

gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

########################

# run once to get the grid

# make a sample to create the base grid etc
samp.size<-250
noise.level<-0.9
samp.ind<-sample(1:length(gendata$x),samp.size)
noise<-noise.level*rnorm(length(samp.ind))
gendata.samp<- list(x=gendata$x[samp.ind],
                    y=gendata$y[samp.ind],
                    z=gendata$z[samp.ind]+noise)


# fit with MDS 2D and no Duchon
base.fit<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=2)


base.fit$mds.dim<-NULL
base.fit$m<-NULL
base.fit$bs<-NULL
base.fit$k<-NULL
base.fit$D.samp<-NULL


# options (as in mds/wt2-bigsim.R)
noise.levels<-c(0.35,0.9,1.55)
sim.size<-200

# for both ML and GCV selection of MDS dimension
for(gam.method in c("ML","GCV.Cp")){
   # results
   real.results<-c()

   # now run the sim
   for(noise.level in noise.levels){
   result<-c()
   
      set.seed(1)
   
      # do each sim
      for(j in 1:sim.size){
         # make samples
         samp.ind<-sample(1:length(gendata$x),samp.size)
         noise<-noise.level*rnorm(length(samp.ind))
         gendata.samp<- list(x=gendata$x[samp.ind],
                             y=gendata$y[samp.ind],
                             z=gendata$z[samp.ind]+noise)
   
   
         this.line<-c()
   
         ind<-c(noise.level,j)
      
         ## run for GCV
         mds.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,
                            old.obj=base.fit,bs="ds",gam.method=gam.method)
         # store some results
         this.line<-rbind(this.line,c(ind,"edf",sum(mds.cd.ds$gam$edf)))
         this.line<-rbind(this.line,c(ind,"mse",sum((mds.cd.ds$pred-gendata$z)^2)))
      
         result<-rbind(result,this.line)
      } # end sims
   
      real.results<-rbind(real.results,result)
   
   } # end noise

write.csv(real.results,paste("bigresults-",gam.method,".csv",sep=""))

} # end methods

#save.image("gcvml.RData")


### plot something
#load("gcvml.RData")
#library(dillhandy)
#library(ggplot2)
#
#real.results<-as.data.frame(real.results)
#real.results[,1]<-as.numeric(as.character(real.results[,1]))
#real.results[,2]<-as.numeric(as.character(real.results[,2]))+2
#real.results[,4]<-as.numeric(as.character(real.results[,4]))
#
#ml.mse<-as.data.frame(pe(real.results,real.results[,3]=="ml.mse"))
#gcv.mse<-as.data.frame(pe(real.results,real.results[,3]=="gcv.mse"))
#ml.score<-as.data.frame(pe(real.results,real.results[,3]=="ml.score"))
#gcv.score<-as.data.frame(pe(real.results,real.results[,3]=="gcv.score"))
#
#names(ml.mse)<-names(gcv.mse)<-names(ml.score)<-names(gcv.score)<-c("sim","dim","name","score")
#
#
#p<-ggplot(ml.score)
#p<-p+geom_boxplot(aes(factor(dim),score))
##p<-p+geom_line(aes(x=dim,y=score,group=sim))
#p
#
#
#p<-ggplot(gcv.score)
#p<-p+geom_boxplot(aes(factor(dim),score))
##p<-p+geom_line(aes(x=dim,y=score,group=sim))
#p
#
#
#
#p<-ggplot(gcv.mse)
#p<-p+geom_boxplot(aes(factor(dim),score))
##p<-p+geom_line(aes(x=dim,y=score,group=sim))
#p
#
#
#p<-ggplot(ml.mse)
#p<-p+geom_boxplot(aes(factor(dim),score))
##p<-p+geom_line(aes(x=dim,y=score,group=sim))
#p
#
#
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
#
#
