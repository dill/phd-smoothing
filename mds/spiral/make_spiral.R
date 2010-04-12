# make the spiral...

# grid maker
source("makesoapgrid.R")

make_spiral<-function(spiral.res=100,n.grid=100){

   phi<-seq(0.5,5*pi,length.out=spiral.res)

   # functions for the spiral
   f1<-function(x){0.5*sqrt(x)*cos(x)}
   f2<-function(x){0.5*sqrt(x)*sin(x)}

   # make the boundary
   offset<-0
   bnd1<-list(x=offset+f1(phi)*1.1,y=offset+f2(phi)*1.1)
   bnd2<-list(x=offset+f1(phi)*0.9,y=offset+f2(phi)*0.9)

   # join the two ends up
   bnd<-list(x=c(bnd1$x,rev(bnd2$x),bnd1$x[1]),
             y=c(bnd1$y,rev(bnd2$y),bnd1$y[1]))

   # create a grid
   sp.grid<-make_soap_grid(bnd,n.grid,mat=TRUE)
   x<-sp.grid$x
   y<-sp.grid$y

   # convert x and y back to find phis 
   phi[x>0 & y>=0]<-atan(y[x>0 & y>=0]/x[x>0 & y>=0])
   phi[x>0 & y<0]<-atan(y[x>0 & y<0]/x[x>0 & y<0])+2*pi
   phi[x<0]<-atan(y[x<0]/x[x<0])+pi
   phi[x==0 & y>0]<-pi/2
   phi[x==0 & y<0]<- 3*pi/2

   r<-sqrt(x^2+y^2)

   z<-rep(0,length(x))

   z<-r*phi+3*pi

   # now make sure that the whole thing is continuous, hack a bit
#   dphi<-seq(0.5,2*pi,length.out=spiral.res)
#   bnd1<-list(x=offset+f1(dphi)*1.1,y=offset+f2(dphi)*1.1)
#   bnd2<-list(x=offset+f1(dphi)*0.9,y=offset+f2(dphi)*0.9)
#   bnd<-list(x=c(bnd1$x,rev(bnd2$x),bnd1$x[1]),
#             y=c(bnd1$y,rev(bnd2$y),bnd1$y[1]))
#   inout<-inSide(bnd,x,y)
#   z[inout]<-z[inout]-2*pi
#   dphi<-seq(2*pi,4*pi,length.out=spiral.res)
#   bnd1<-list(x=offset+f1(dphi)*1.1,y=offset+f2(dphi)*1.1)
#   bnd2<-list(x=offset+f1(dphi)*0.9,y=offset+f2(dphi)*0.9)
#   bnd<-list(x=c(bnd1$x,rev(bnd2$x),bnd1$x[1]),
#             y=c(bnd1$y,rev(bnd2$y),bnd1$y[1]))
#   inout<-inSide(bnd,x,y)
#   z[inout]<-z[inout]-pi

#   ind<-


   #z[r>1]<-z[r>1]+pi

   # create the data frame
   dat<-data.frame(x=x,y=y,z=z)

   # create a matrix for plotting
   mat<-sp.grid$mat
   mat[!is.na(mat)]<-z


   return(list(dat=dat,bnd=bnd,mat=mat))


}

### test code
#spir.dat<-make_spiral()
#image(z=spir.dat$mat,x=seq(min(spir.dat$bnd$x),max(spir.dat$bnd$x),len=dim(spir.dat$mat)[2]),
#      y=seq(min(spir.dat$bnd$y),max(spir.dat$bnd$y),len=dim(spir.dat$mat)[1]),
#      col=heat.colors(100))
#lines(spir.dat$bnd)



