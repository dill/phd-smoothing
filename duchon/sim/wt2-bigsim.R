# do large scale simulations for wt2 - with Duchon splines
# David Lawrence Miller 2009-2011.

library(mdspack)

source("wt2-smooth-test.R")

######################################################
# The following models get tested:
#
#     * tprs
#     * mds+tp
#     * mds+cr
#     * mds+tp (3d)
#     * mds+tp + penalty adjustments
#     * soap
#     * 3D with Duchon splines 


######################################################
# OPTIONS
sim.size<-200
samp.sizes<-c(250,500)
# noise levels = 0.35,0.9,1.55
# snr = 0.95,0.75,0.50
#noise.level<-1.55



######################################################
# initial setup
# this pre-calculates some things to make life easier

## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata<-read.csv("wt2truth.csv",header=TRUE)

gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

na.ind<-!(is.na(gendata$x)&is.na(gendata$y)&is.na(gendata$z))

gendata<-list(x=gendata$x[na.ind],
               y=gendata$y[na.ind],
               z=gendata$z[na.ind])


##########################################################
### setup the soap knots
knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
insideknots<-inSide(bnd,knots.x,knots.y)
soap.knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
soap.knots<-pe(soap.knots,-c(55,96,108))


########################################################
## MDS setup
## create the sample
samp.ind<-sample(1:length(gendata$x),200)
samp<-list(x=gendata$x[samp.ind],
           y=gendata$y[samp.ind])
base.mds<-gam.mds(samp,gendata,bnd,mds.dim=2,grid.res=120)
base.mds$D.samp<-NULL # make it so we recalculate sample D matrix
                      # every time...
base.mds$mds.dim<-NULL
base.mds$m<-NULL
base.mds$bs<-NULL
base.mds$k<-NULL


##########################################################
# actually run the sim now... 

#noiselevels<-c(0.35,0.9,1.55)
noiselevels<-c(0.9,1.55)

for(samp.size in samp.sizes){
   for(noise.level in noiselevels){
   
      cat("Noise level=",noise.level,"\n")
   
      # set up some containers
      res.mse<-matrix(NA,sim.size,7)
      res.edf<-matrix(NA,sim.size,7)
      
      # do the sims
      for(i in 1:sim.size){
         res<-wt2_smooth_test(samp.size,noise.level,plot.it=FALSE,
                                gendata,bnd,base.mds,soap.knots)
         res.mse[i,]<-res$mse
         #res.edf[i,]<-res$edf
      }
      
      # put it all in a nice data frame
      col.names<-c("tprs","mds2d","mds3dds","mds85","mds90","mds95","soap")
      res.mse<-as.data.frame(res.mse)
      names(res.mse)<-col.names
      #res.edf<-as.data.frame(res.edf)
      #names(res.edf)<-col.names
      
      # write the files...
      write.csv(res.mse,file=paste("wt2-mse-",samp.size,"-",noise.level,".csv",sep=""))
      #write.csv(res.edf,file=paste("wt2-edf-",samp.size,"-",noise.level,".csv",sep=""))
   }
}
