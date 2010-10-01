# attempt to duplicate the simulation settings in 
# Wang and Ranalli (2007)
# Copyright David Lawrence Miller 2010
#source("mds.R")
#source("tps.R")

source("wr-wrapper.R")

# import fields for the cover.design() function
library(fields)

set.seed(12)

# as in the paper
samp.size<-100
noise.level<-0.05
n.knots<-40
replicates<-200


## create a boundary...
bnd <- fs.boundary()
ee<-5
keepers<-c(seq(1,19,ee),20,21,seq(22,59,ee),60,61,seq(62,99,ee),100,
            101,seq(102,139,ee),140,141,seq(142,159,ee),160)
bnd<-pe(bnd,keepers)
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
# create points within the boundary 
#m<-50;n<-30 # with every 6th point kept
m<-45;n<-30 # 950 points with full bnd or 943 with partial... 
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]

# truth
z.truth<-fs.test(xx,yy)

# pre-calculate the distances

xp<-matrix(c(xx,yy),length(xx),2)

res<-list(tps=matrix(NA,replicates,length(xx)),
          wr=matrix(NA,replicates,length(xx)))

for(i in 1:replicates){

   # make the sample
   samp.ind<-sample(1:length(xx),samp.size)
   
   # add noise
   noise<-rnorm(samp.size)*noise.level
   
   samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                         z=fs.test(xx[samp.ind],yy[samp.ind])+noise)
   

   # knot selection
   xk<-cover.design(matrix(c(samp.data$x,samp.data$y),length(samp.data$x),2),n.knots)
   xk<-matrix(c(xk[,1],xk[,2]),length(xk[,1]),2)

   ### fit with tps

   beta.tps<-fit.tps(samp.data$z,cbind(samp.data$x,samp.data$y),xk)
   pred.tps<-eval.tps(xp,beta.tps,xk)

   ### fit with wr
   
   beta.wr<-wr(samp.data,list(x=xk[,1],y=xk[,2]),bnd)
   pred.wr<-wr.pred(list(x=xp[,1],y=xp[,2]),list(x=xk[,1],y=xk[,2]),beta.wr)


   res$tps[i,]<-pred.tps
   res$wr[i,]<-pred.wr

   ### fit with mds

}

# save this
save.image(file="ram-wr-comp.RData")


# calculate MSE


# calculate MSSE


# calculate bias


# calculate artifactiness

