# function to create a grid for soap
make_soap_grid<-function(bnd,n.grid,mat=FALSE,log=FALSE,delta=FALSE){

   # first if the boundary is a list, turn it into a data.frame
   if(is.list(bnd)){
      bnd<-as.data.frame(bnd)
   }

   # set the grid size
   if(length(n.grid)!=ncol(bnd)){
      n.grid<-rep(n.grid,ncol(bnd))
   }

   # min and max values of the boundary (but not on the boundary)
   min.vals<-apply(bnd,2,min,na.rm=TRUE)
   max.vals<-apply(bnd,2,max,na.rm=TRUE)

   # create the grid
   #for(i in 1:length(n.grid)){
   #   x<-seq(min.vals[i],max.vals[i],n.grid[i])
   #   xx<-rep(x,ngrid[i])
   #}

   xm <- seq(min.vals[1],max.vals[1],length=n.grid[1])
   yn<-seq(min.vals[2],max.vals[2],length=n.grid[2])
   xx <- rep(xm,n.grid[2])
   yy<-rep(yn,rep(n.grid[1],n.grid[2]))


   onoff<-inSide(bnd,xx,yy)
   xx<-xx[onoff]
   yy<-yy[onoff]

   ret<-list(x=xx,y=yy)

   return(ret)
}
