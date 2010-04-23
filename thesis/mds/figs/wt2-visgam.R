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

grid.mds<-grid.mds$points

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

### Now do some fitting and prediction
### mapping
b.mapped<-gam(z~s(x,y,k=49),data=samp.data)

# plot using vis.gam
vis.gam(b.mapped,plot.type="contour",too.far=0.04,main="",n.grid=100)
points(samp.mds,pch=19,cex=0.3)



