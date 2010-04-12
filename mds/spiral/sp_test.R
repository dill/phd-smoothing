# evaluate a location on the spiral
sp_test<-function(x,y){

   z<-rep(0,length(x))

   z[x>0]<-atan(y[x>0]/x[x>0])+pi
   z[x<0]<-atan(y[x<0]/x[x<0])
   z[x==0 & y>0]<-pi/2
   z[x==0 & y<0]<-pi/2

   return(z)

}
