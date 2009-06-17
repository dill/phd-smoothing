source("mds.R")

library(soap)

## create a boundary...
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))




### predict over a grid
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]


# create the prediction grid
new.data<-data.frame(x=xx,y=yy,z=fs.test(xx,yy))

# just go from where we left off last time
#xx<-xx[54:length(xx)];yy<-yy[54:length(yy)]


# map the prediction grid
D<-create_distance_matrix(xx,yy,bnd,logfile="ramsay-big-log.txt")





new.coords<-cmdscale(D)
new.data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=new.data$z)


# write to file
complete.data<-data.frame(x=new.data$x,y=new.data$y,
                          xmds=new.data.mapped$x,ymds=new.data.mapped$y,
                          z=new.data.mapped$z)
write.csv(complete.sample.data,file="ramsay-sample.csv")


### probably want to add some noise at this point
## Simulate some fitting data, inside boundary...
n.samp<-250
samp.ind<-sample(1:length(xx),n.samp)
noise<-rnorm(n.samp)*2
samp.data.n<-data.frame(x=xx[samp.ind],y=yy[samp.ind],z=new.data$z[samp.ind]+noise)
samp.data.t<-data.frame(x=new.data.mapped$x[samp.ind],y=new.data.mapped$y[samp.ind],z=new.data.mapped$z[samp.ind]+noise)




####

# plot
par(mfrow=c(2,2))

# truth
z.truth<-matrix(NA,m,yn)
z.truth[onoff]<-new.data$z
image(xm,yn,z.truth,col=heat.colors(100),xlab="x",ylab="y",main="truth")
contour(xm,yn,z.truth,levels=seq(-5,5,by=.25),add=TRUE)

### mapping
b.mapped<-gam(z~s(x,y,k=49),data=samp.data.t)
fv.mapped <- predict(b.mapped,newdata=new.data.mapped)

pred.mat<-matrix(NA,m,n)
pred.mat[onoff]<-fv.mapped

image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="MDS")
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)


### normal tprs
b.tprs<-gam(z~s(x,y,k=49),data=samp.data.n)
fv.tprs <- predict(b.tprs,newdata=new.data)

pred.mat<-matrix(NA,m,n)
pred.mat[onoff]<-fv.tprs

image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tprs")
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)


### soap
# create some internal knots...
knots <- data.frame(x=rep(seq(-.5,3,by=.5),4),
                    y=rep(c(-.6,-.3,.3,.6),rep(8,4)))
knots.ind<-inSide(bnd,x=knots$x,y=knots$y)
knots<-list(x=knots$x[knots.ind],y=knots$y[knots.ind])
b.soap<-gam(z~s(x,y,k=20,bs="so",xt=list(bnd=fsb)),knots=knots,data=samp.data.n)
fv.soap<-predict(b.soap,newdata=new.data,block.size=-1)

pred.mat<-matrix(NA,m,n)
pred.mat[onoff]<-fv.soap

image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="soap")
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)


### calculate MSEs
cat("tprs MSE=",mean((fv.tprs-new.data$z)^2,na.rm=TRUE),"\n")
cat("soap MSE=",mean((fv.soap-new.data$z)^2,na.rm=TRUE),"\n")
cat("mapped MSE=",mean((fv.mapped-new.data$z)^2,na.rm=TRUE),"\n")



