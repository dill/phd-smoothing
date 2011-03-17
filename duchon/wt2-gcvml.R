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
      # store the EDF for the GCV model
      this.line<-rbind(this.line,c(ind,"gcv.edf",sum(mds.cd.ds$gam$edf)))

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

save.image("gcvml.RData")

