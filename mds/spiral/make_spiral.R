# make the spiral...

make_spiral<-function(spiral.res){


   phi<-seq(0.5,5*pi,length.out=spiral.res)

   # functions for the spiral
   f1<-function(x){0.5*sqrt(x)*cos(x)}
   f2<-function(x){0.5*sqrt(x)*sin(x)}


   bnd1<-list(x=f1(phi)*1.1,y=f2(phi)*1.1)
   bnd2<-list(x=f1(phi)*0.9,y=f2(phi)*0.9)

   bnd<-list(x=c(bnd1$x,rev(bnd2$x),bnd1$x[1]),
             y=c(bnd1$y,rev(bnd2$y),bnd1$y[1]))

   # change phi so that the data only exists inside the boundary
   phi<-phi[phi>0.5]
   phi<-phi[-length(phi)]

   dat<-list(x=f1(phi),y=f2(phi))

   return(list(dat=dat,bnd=bnd))


}

plot(make_spiral(100)$bnd,type="l",col="red")
lines(make_spiral(100)$dat)


