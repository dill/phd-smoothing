source("mds.R")
source("wood.R")
source("utils.R")

library(soap)

## create a boundary...
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))

## Simulate some fitting data, inside boundary...
n<-250
x <- runif(n)*5-1; y<-runif(n)*2-1
z <- fs.test(x,y,b=1)
ind <- inSide(bnd,x=x,y=y) ## remove outsiders
x<-x[ind];y <- y[ind];z <- z[ind]
samp.data<-data.frame(x=x,y=y,z=z)

D<-create_distance_matrix(x,y,bnd,logfile="ramsey-small-log.txt")

new.coords<-cmdscale(D)

data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=z)




# write to file
complete.sample.data<-data.frame(x=samp.data$x,y=samp.data$y,
                                 xmds=data.mapped$x,ymds=data.mapped$y,
                                 z=samp.data$z)
write.csv(complete.sample.data,file="ramsay-sample.csv")





### predict over a grid
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]


# create the prediction grid
new.data<-data.frame(x=xx,y=yy,z=fs.test(xx,yy))

# map the prediction grid
D<-create_distance_matrix(xx,yy,bnd,logfile="ramsey-big-log.txt")



### doesn't work
x=c( -0.2840909 , 1.556818 )
y=c( 0.08333333 , -0.75 )





new.coords<-cmdscale(D)
new.data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=new.data$z)


# write to file
complete.data<-data.frame(x=new.data$x,y=new.data$y,
                          xmds=new.data.mapped$x,ymds=new.data.mapped$y,
                          z=new.data.mapped$z)
write.csv(complete.sample.data,file="ramsay-sample.csv")


### probably want to add some noise at this point




####

# plot
par(mfrow=c(2,2))

# truth
image(xm,yn,new.data$z,col=heat.colors(100),xlab="x",ylab="y",main="truth")
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)

### mapping
b.mapped<-gam(z~s(x,y,k=49),data=data.mapped)
fv.mapped <- predict(b.mapped,newdata=new.data.mapped)

pred.mat<-matrix(NA,m,n)
pred.mat[onoff]<-fv.mapped

image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="MDS")
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)


### normal tprs
b.tprs<-gam(z~s(x,y,k=49),data=samp.data)
fv.tprs <- predict(b.tprs,newdata=new.data)

pred.mat<-matrix(fv.mapped,m,n)
pred.mat[!onoff]<-NA

image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tprs")
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)

### soap

# create a boundary...
fsb <- list(fs.boundary())
# create some internal knots...
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                    w=rep(c(-.6,-.3,.3,.6),rep(8,4)))
b.soap<-gam(z~s(x,y,k=49,bs="so",xt=list(bnd=fsb)),knots=knots,data=samp.data)
fv.soap<-predict(b.soap,newdata=new.data,block.size=-1)

pred.mat<-matrix(fv.mapped,m,n)
pred.mat[!onoff]<-NA

image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tprs")
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)




