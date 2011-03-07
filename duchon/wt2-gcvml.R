# comparison of using GCV and ML for selecting the number 
# of dimensions to project into


library(mdspack)

# let's do this in parallel
library(foreach)
library(doMC)
options(cores=6)
registerDoMC()


#set.seed(123)
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
#gendata<-read.csv("wt2truth-new.csv",header=TRUE) # new with more splodges
gendata<-read.csv("wt2truth.csv",header=TRUE) # old 
onoff<-gendata$inside==1

gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

########################

# run once to get the grid

# make a sample
samp.size<-500
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


## storage
n.dims<-20
n.runs<-60
dim.list<-seq(3,n.dims,by=1)

real.results<-c()

# now run many times

for(i in 1:n.runs){
   # make samples
   samp.ind<-sample(1:length(gendata$x),samp.size)
   noise<-noise.level*rnorm(length(samp.ind))
   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind]+noise)
   
## parallel
#   for(j in 1:length(dim.list)){
   result<-foreach(j = 1:length(dim.list),.combine=rbind) %dopar% {

      this.line<-c()

      ind<-c(i,j)

      expl<-dim.list[j]

      ## run for GCV
      mds.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,
                         old.obj=base.fit,bs="ds",gam.method="GCV.Cp")
      # store some results
      this.line<-rbind(this.line,c(ind,"gcv.score",mds.cd.ds$gam$gcv.ubre))
      this.line<-rbind(this.line,c(ind,"gcv.aic",mds.cd.ds$gam$aic))
      this.line<-rbind(this.line,c(ind,"gcv.mse",sum((mds.cd.ds$pred-gendata$z)^2)))

      ## run for ML
      mds.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,
                         old.obj=base.fit,bs="ds",gam.method="ML")

      # store some results
      this.line<-rbind(this.line,c(ind,"ml.score",mds.cd.ds$gam$gcv.ubre))
      this.line<-rbind(this.line,c(ind,"ml.aic",mds.cd.ds$gam$aic))
      this.line<-rbind(this.line,c(ind,"ml.mse",sum((mds.cd.ds$pred-gendata$z)^2)))

      ## run for REML
      mds.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,
                         old.obj=base.fit,bs="ds",gam.method="REML")

      # store some results
      this.line<-rbind(this.line,c(ind,"reml.score",mds.cd.ds$gam$gcv.ubre))
      this.line<-rbind(this.line,c(ind,"reml.aic",mds.cd.ds$gam$aic))
      this.line<-rbind(this.line,c(ind,"reml.mse",sum((mds.cd.ds$pred-gendata$z)^2)))



      #return(this.line)
      this.line
   }
## end parallel

   real.results<-rbind(real.results,result)

}

#plot(res,xlab="MDS dimension",ylab="GCV Score",pch=19,cex=0.3)
#abline(h=min(res[,2]),col="red")

#save.image("gcvml.RData")

########################################################################

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


