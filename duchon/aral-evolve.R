# Aral Sea data set analysis - evolce Duchon...

# libraries
library(mgcv)
library(soap)

library(dillhandy)
library(mdspack)

# load the data and boundary
aral<-read.csv("aral.dat",sep=" ")
bnd<-read.csv("aralbnd.csv")

zlims<-c(1.905461, 19.275249)

# first cut out the crap using inSide
onoff<-inSide(bnd,aral$lo,aral$la)

# converstion to km
aral.km<-latlong2km(aral$lo[onoff],aral$la[onoff],59.5,45)

aral.dat<-data.frame(x=aral.km$km.e,
                     y=aral.km$km.n,
                     chl=as.numeric(aral$chl[onoff]))

# convert boundary to northings and eastings
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)
bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)


# prediction grid
gm<-50;gn<-50
gxm <- seq(min(aral.dat$x),max(aral.dat$x),length=gm)
gyn<-seq(min(aral.dat$y),max(aral.dat$y),length=gn)
gxx <- rep(gxm,gn)
gyy<-rep(gyn,rep(gm,gn))
pred.onoff<-inSide(bnd,gxx,gyy)
pred.grid<-data.frame(x=gxx[pred.onoff],y=gyy[pred.onoff])

######################################################################
# plot setup
par(mfrow=c(2,4),las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

# set the x and y values for the image plot
aral.lab<-latlong2km(unique(sort(aral$lo)),unique(sort(aral$la)),59.5,45)
# set the plot limits
xlims<-c(min(aral.dat$x)-10,max(aral.dat$x)+10)
ylims<-c(min(aral.dat$y)-10,max(aral.dat$y)+10)

######################################################################
#### plot some raw data

aral$chl[!onoff]<-NA
image(z=matrix(aral$chl,46,46),x=aral.lab$km.e,y=aral.lab$km.n,
      asp=1,main="raw data",xlab="km (East)",ylab="km (North)",xlim=xlims,ylim=ylims)
lines(bnd,lwd=2)

######################################################################
#### fit a thin plate model
tp.fit<-gam(chl~s(x,y,k=70),data=aral.dat,family=Gamma(link="log"))


tp.pred<-predict(tp.fit,newdata=pred.grid,type="response")
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-tp.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),main="tprs",xlab="km (East)",ylab="km (North)",xlim=xlims,ylim=ylims,asp=1)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)

######################################################################
#### soap 

### VVVVVVVVVVVVVVVVVVV this doesn't work (?!)
# first setup the knots
#s.knots<-create_refgrid(bnd,140)
#s.knots$nrefx<-NULL
#s.knots$nrefy<-NULL
#s.knots<-as.data.frame(s.knots)

#s.knots<-make_soap_grid(bnd,17)
s.knots<-make_soap_grid(bnd,c(12,12))

soap.fit<-gam(chl~s(x,y,k=49,bs="so",xt=list(bnd=list(bnd))),knots=s.knots,
            family=Gamma(link="log"),data=aral.dat)

# prediction
soap.pred<-predict(soap.fit,newdata=pred.grid,type="response")
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-soap.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),xlab="km (East)",ylab="km (North)",main="soap",xlim=xlims,ylim=ylims,asp=1)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)


######################################################################
#### MDS
# mds grid
m<-20;n<-20
xm <- seq(min(aral.dat$x),max(aral.dat$x),length=m)
yn<-seq(min(aral.dat$y),max(aral.dat$y),length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
grid.onoff<-inSide(bnd,xx,yy)
mds.grid<-data.frame(x=xx[grid.onoff],y=yy[grid.onoff])

# actually do the MDS
D<-create_distance_matrix(mds.grid$x,mds.grid$y,bnd,faster=1)
#####################################




grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)

# create the data frame and fit the model
aral.mds<-insert.mds(aral.dat,mds.grid,grid.mds,bnd,faster=1)
aral.mds<-data.frame(x=aral.mds[,1],
                     y=aral.mds[,2],
                     chl=aral.dat$chl)

# fit the model
mds.fit<-gam(chl~s(x,y,k=70),data=aral.mds,family=Gamma(link="log"))

# mds prediction grid
pred.grid.mds<-insert.mds(pred.grid,mds.grid,grid.mds,bnd,faster=1)
pred.grid.mds<-data.frame(x=pred.grid.mds[,1],
                          y=pred.grid.mds[,2])

# do the prediction
mds.pred<-predict(mds.fit,newdata=pred.grid.mds,type="response")

# plot
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-mds.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),main="mds",xlab="km (East)",ylab="km (North)",xlim=xlims,ylim=ylims,asp=1)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)





######################################################################

### 3D + MDS
grid.mds<-cmdscale(D,eig=TRUE,k=3,x.ret=TRUE)

# create the data frame and fit the model
aral.mds<-insert.mds(aral.dat,mds.grid,grid.mds,bnd,faster=1)
aral.mds<-data.frame(x=aral.mds[,1],
                     y=aral.mds[,2],
                     z=aral.mds[,3],
                     chl=aral.dat$chl)


# mds prediction grid
pred.grid.mds<-insert.mds(pred.grid,mds.grid,grid.mds,bnd,faster=1)
pred.grid.mds<-data.frame(x=pred.grid.mds[,1],
                          y=pred.grid.mds[,2],
                          z=pred.grid.mds[,3])

# fit the model
mds3.fit<-gam(chl~s(x,y,z,k=140),data=aral.mds,family=Gamma(link="log"))

# do the prediction
mds3.pred<-predict(mds3.fit,newdata=pred.grid.mds,type="response")

pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-mds3.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),
      xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,asp=1,zlim=zlims,main="3D tprs")
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)




######################################################################
### Duchon

# 3d Duchon


grid.mds<-cmdscale(D,eig=TRUE,k=3,x.ret=TRUE)

# create the data frame and fit the model
aral.mds<-insert.mds(aral.dat,mds.grid,grid.mds,bnd,faster=1)
aral.mds<-data.frame(x=aral.mds[,1],
                     y=aral.mds[,2],
                     z=aral.mds[,3],
                     w=aral.mds[,4],
                     chl=aral.dat$chl)


# mds prediction grid
pred.grid.mds<-insert.mds(pred.grid,mds.grid,grid.mds,bnd,faster=1)
pred.grid.mds<-data.frame(x=pred.grid.mds[,1],
                          y=pred.grid.mds[,2],
                          z=pred.grid.mds[,3],
                          w=pred.grid.mds[,4])
#########
# fit the model
mds4d.fit<-gam(chl~s(w,x,y,z,k=140,bs="ds",m=c(3,4/2-1)),data=aral.mds,family=Gamma(link="log"))

# do the prediction
mds4d.pred<-predict(mds4d.fit,newdata=pred.grid.mds,type="response")


pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-mds4d.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),
      xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,asp=1,zlim=zlims,main="Duchon - 3D")
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)






# 4d Duchon

grid.mds<-cmdscale(D,eig=TRUE,k=4,x.ret=TRUE)

# create the data frame and fit the model
aral.mds<-insert.mds(aral.dat,mds.grid,grid.mds,bnd,faster=1)
aral.mds<-data.frame(x=aral.mds[,1],
                     y=aral.mds[,2],
                     z=aral.mds[,3],
                     w=aral.mds[,4],
                     chl=aral.dat$chl)


# mds prediction grid
pred.grid.mds<-insert.mds(pred.grid,mds.grid,grid.mds,bnd,faster=1)
pred.grid.mds<-data.frame(x=pred.grid.mds[,1],
                          y=pred.grid.mds[,2],
                          z=pred.grid.mds[,3],
                          w=pred.grid.mds[,4])
#########
# fit the model m=c(3,4/2-1))
mds4d.fit<-gam(chl~s(w,x,y,z,k=140,bs="ds",m=c(3,3/2-1)),data=aral.mds,family=Gamma(link="log"))

# do the prediction
mds4d.pred<-predict(mds4d.fit,newdata=pred.grid.mds,type="response")


pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-mds4d.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),
      xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,asp=1,zlim=zlims,main="Duchon 4D s=2")
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)
 

########
# fit the model m=c(3,4/3-1))
mds4d.fit<-gam(chl~s(w,x,y,z,k=140,bs="ds",m=c(3,4/3-1)),data=aral.mds,family=Gamma(link="log"))

# do the prediction
mds4d.pred<-predict(mds4d.fit,newdata=pred.grid.mds,type="response")


pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-mds4d.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),
      xlab="km (East)",ylab="km (North)",
      xlim=xlims,ylim=ylims,asp=1,zlim=zlims,main="Duchon 4D s=3")
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=pretty(zlims,15))
lines(bnd,lwd=2)

