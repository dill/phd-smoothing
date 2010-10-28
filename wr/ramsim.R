# attempt to duplicate the simulation settings in 
# Wang and Ranalli (2007)
# Copyright David Lawrence Miller 2010

source("wr-wrapper.R")
library(fields) # import fields for the cover.design() function

set.seed(12)

# as in the paper
samp.size<-100
noise.level<-5 #0.05, 0.5
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

xp<-matrix(c(xx,yy),length(xx),2)
pred.data<-list(x=xx,y=yy)

# make the grid to add to, also prediction points
my.grid<-list(x=xx,y=yy)
D.grid<-create_distance_matrix(xx,yy,bnd)
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
pred.mds<-list(x=grid.mds$points[,1],y=grid.mds$points[,2])

# results storage
res<-list(tps=matrix(NA,replicates,length(xx)),
          wr=matrix(NA,replicates,length(xx)),
          mdstps=matrix(NA,replicates,length(xx)),
          mdstprs=matrix(NA,replicates,length(xx)),
          soap=matrix(NA,replicates,length(xx)))

# actually do the simulations
for(i in 1:replicates){

   # make the sample
   samp.ind<-sample(1:length(xx),samp.size)
   
   # add noise
   noise<-rnorm(samp.size)*noise.level

   # sample data set   
   samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                         z=fs.test(xx[samp.ind],yy[samp.ind])+noise)
   
   # map the sample   
   samp.mds<-insert.mds(samp.data,my.grid,grid.mds,bnd)
   samp.mds<-data.frame(x=samp.mds[,1],y=samp.mds[,2],z=samp.data$z)


   # knot selection
   xk<-cover.design(matrix(c(samp.data$x,samp.data$y),length(samp.data$x),2),n.knots)
   xk<-matrix(c(xk[,1],xk[,2]),length(xk[,1]),2)


   #################################
   # actual model fitting below here
   ### fit with tps
   beta.tps<-fit.tps(samp.data$z,cbind(samp.data$x,samp.data$y),xk)
   pred.tps<-eval.tps(xp,beta.tps,xk)

   ### fit with wr
   beta.wr<-wr(samp.data,list(x=xk[,1],y=xk[,2]),bnd)
   pred.wr<-wr.pred(list(x=xp[,1],y=xp[,2]),list(x=xk[,1],y=xk[,2]),beta.wr)

   ### fit with tps
   beta.tps<-fit.tps(samp.mds$z,cbind(samp.mds$x,samp.mds$y),xk)
   pred.tps<-eval.tps(xp,beta.tps,xk)

   ### fit with tprs
   b.mapped<-gam(z~s(x,y,k=100),data=samp.mds)
   pred.tprs<-predict(b.mapped,newdata=pred.mds)

   ### fit with soap
   knots <- data.frame(x=rep(seq(-.5,3,by=.5),4),
                       y=rep(c(-.6,-.3,.3,.6),rep(8,4)))
   knots.ind<-inSide(bnd,x=knots$x,y=knots$y)
   knots<-list(x=knots$x[knots.ind],y=knots$y[knots.ind])
   b.soap<-gam(z~s(x,y,k=39,bs="so",xt=list(bnd=list(bnd))),knots=knots,data=samp.data)
   pred.soap<-predict(b.soap,newdata=pred.data,block.size=-1)

   res$tps[i,]<-pred.tps
   res$wr[i,]<-pred.wr
   res$mdstps[i,]<-pred.tps
   res$mdstprs[i,]<-pred.tprs
   res$soap[i,]<-pred.soap
}

# save this
save.image(file=paste("ramsim-",noise.level,"-",samp.size,"-results.RData",sep=""))

