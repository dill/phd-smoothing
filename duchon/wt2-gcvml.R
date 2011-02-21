# comparison of using GCV and ML for selecting the number 
# of dimensions to project into


library(mdspack)

#set.seed(123)
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata<-read.csv("wt2truth-new.csv",header=TRUE)
onoff<-gendata$inside==1

gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

zlims<-c(min(gendata$z),max(gendata$z))


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
n.dims<-22
n.runs<-100
gcv.res<-matrix(NA,n.runs,n.dims)
ml.res<-matrix(NA,n.runs,n.dims)
gcv.mse.res<-matrix(NA,n.runs,n.dims)
ml.mse.res<-matrix(NA,n.runs,n.dims)

dim.list<-seq(3,n.dims,by=1)

# now run many times

for(i in 1:n.runs){
   # make samples
   samp.ind<-sample(1:length(gendata$x),samp.size)
   noise<-noise.level*rnorm(length(samp.ind))
   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind]+noise)
   
   for(j in 1:length(dim.list)){

      expl<-dim.list[j]

      ## run for GCV
      mds.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,
                         old.obj=base.fit,bs="ds",gam.method="GCV.Cp")
      # store some results
      gcv.res[i,j]<-mds.cd.ds$gam$gcv.ubre
      gcv.mse.res[i,j]<-sum((mds.cd.ds$pred-gendata$z)^2)


      ## run for ML
      mds.cd.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=expl,
                         old.obj=base.fit,bs="ds",gam.method="ML")

      # store some results
      ml.res[i,j]<-mds.cd.ds$gam$gcv.ubre
      ml.mse.res[i,j]<-sum((mds.cd.ds$pred-gendata$z)^2)

   }

}

#plot(res,xlab="MDS dimension",ylab="GCV Score",pch=19,cex=0.3)
#abline(h=min(res[,2]),col="red")



## plot something



