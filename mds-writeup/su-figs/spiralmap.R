# make spiralmap.pdf


source("mds.R")
source("spiral/make_spiral.R")
samp.size=250
noise.level=0.05
spir.dat<-make_spiral(25,50)
bnd<-spir.dat$bnd
xx<-spir.dat$dat$x
yy<-spir.dat$dat$y
spir.grid<-make_spiral(25,n.grid=15)
my.grid<-list(x=spir.grid$dat$x,y=spir.grid$dat$y)
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=1)
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
samp.ind<-sample(1:length(xx),samp.size)

# add noise
noise<-rnorm(samp.size)*noise.level

samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                      z=fs.test(xx[samp.ind],yy[samp.ind])+noise)

# insert the sample
samp.mds<-insert.mds(samp.data,my.grid,grid.mds,bnd,faster=1)

plot(samp.data$x,samp.data$y,xlab="x",ylab="y",asp=1)
plot(samp.mds,xlab="x*",ylab="y*",asp=1)

