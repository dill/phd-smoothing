# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")
 
samp.size=250
noise.level=0.05
 
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

# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
onoff<-inSide(bnd.neg,-gendata$x,-gendata$y)

gendata<-list(x=gendata$x[onoff],
               y=gendata$y[onoff],
               z=gendata$z[onoff])

# create the sample index
samp.ind<-sample(1:length(gendata$x),samp.size)

## create the sample
gendata.samp<- list(x=gendata$x[samp.ind],
                    y=gendata$y[samp.ind],
                    z=gendata$z[samp.ind])



gendata<-list(x=gendata$x[-samp.ind],
               y=gendata$y[-samp.ind],
               z=gendata$z[-samp.ind])

# create the grid
source("wt2-create-grid.R")
my.grid<-wt2_create_grid()

## do the MDS on the grid 
# create D
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd)

# perform mds on D
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2)

# sample points insertion
samp.mds<-insert.mds(gendata.samp,my.grid,grid.mds,bnd)

# prediction points insertion
pred.mds<-insert.mds(gendata,my.grid,grid.mds,bnd)

# add noise
noise<-noise.level*rnorm(length(samp.ind))
#> summary(gendata$z)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
#0.000000 0.000236 0.269300 0.276300 0.479600 0.850000

# mapped sample data
samp.data<-list(x=c(),y=c(),z=c())
samp.data$x<-samp.mds[,1]
samp.data$y<-samp.mds[,2]
samp.data$z<-gendata.samp$z+noise

# non-mapped sample data
nsamp.data<-list(x=c(),y=c(),z=c())
nsamp.data$x<-gendata.samp$x
nsamp.data$y<-gendata.samp$y
nsamp.data$z<-gendata.samp$z+noise

### create prediction data
# non-mapped prediction data
npred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                y=rep(0,length(gendata$x)+length(samp.data$y)))
npred.data$x[-samp.ind]<-gendata$x
npred.data$y[-samp.ind]<-gendata$y
npred.data$x[samp.ind]<-nsamp.data$x
npred.data$y[samp.ind]<-nsamp.data$y


# put this in the correct format
pred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                y=rep(0,length(gendata$x)+length(samp.data$y)))
pred.data$x[-samp.ind]<-pred.mds[,1]
pred.data$y[-samp.ind]<-pred.mds[,2]
pred.data$x[samp.ind]<-samp.mds[,1]
pred.data$y[samp.ind]<-samp.mds[,2]


### P-splines knot stuff here
source("ps/wt2-create-grid.R")
knots<-wt2_create_grid()
knots.mds<-insert.mds(knots,my.grid,grid.mds,bnd)
knots.mds<-list(x=knots.mds[,1],y=knots.mds[,2])

# extra knots
x.extra<-c(max(knots.mds$x)+0.25,max(knots.mds$x)+0.25,
           min(knots.mds$x)-0.25,min(knots.mds$x)-0.25)
y.extra<-c(max(knots.mds$y)+0.25,min(knots.mds$y)-0.25,
            max(knots.mds$y)+0.25,min(knots.mds$y)-0.25)


knots.mds<-list(x=c(knots.mds[,1],x.extra),y=c(knots.mds[,2],y.extra))



### Now do some fitting and prediction
### mapping
b.mapped<-gam(z~s(x,y,k=49),data=samp.data)
fv <- predict(b.mapped,newdata=pred.data)
   
 
# create the image
gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
ind<-c(1:length(gendata.ind$x))
pred.mat<-rep(NA,length(gendata.ind$x))
ind<-ind[gendata.ind$inside==1]
na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
ind<-ind[na.ind]
ind<-ind[onoff]
 
 
# plot for truth, mds, tprs and soap
par(mfrow=c(1,2))

# axis scales
xscale<-seq(min(gendata$x),max(gendata$x),length.out=50)
yscale<-seq(min(gendata$y),max(gendata$y),length.out=50)

pred.mat<-rep(NA,length(gendata.ind$x))
pred.mat[ind]<-gendata.ind$z[ind]
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="truth",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
contour(xscale,yscale,pred.mat,add=T)

pred.mat<-rep(NA,length(gendata.ind$x))
pred.mat[ind]<-fv
pred.mat<-matrix(pred.mat,50,50)
image(xscale,yscale,pred.mat,main="mds",asp=1,las=1,xlab="x",ylab="y",col=heat.colors(100))
contour(xscale,yscale,pred.mat,add=T)
   
### calculate MSEs
#mses<-list(mds=mean((fv-gendata.ind$z[ind])^2,na.rm=T),
#           tprs=mean((fv.tprs-gendata.ind$z[ind])^2,na.rm=T),
#           soap=mean((fv.soap-gendata.ind$z[ind])^2,na.rm=T))

# print them
#cat("mds MSE=" ,mses$mds,"\n")
#cat("tprs MSE=",mses$tprs,"\n")
#cat("soap MSE=",mses$soap,"\n")
 
