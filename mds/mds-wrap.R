# wrapper function for C code
woodpath<-function(xpoints,ypoints,bnd,start){

   # put everything in the right format
   xbnd<-bnd$x
   ybnd<-bnd$y
   nbnd<-length(xbnd)
   len<-length(xpoints)

   if(is.na(start)){
      # when we do full MDS then we just compute the upper triangle
      pl<-(len*(len-1)/2)
      insert<-FALSE # used later
      start<-0
   }else{
      # in the insertion case, start gives the length of the old points
      # here we are calculating a length(old.points)*length(new.points) matrix
      pl<-len-start # length of new points
      pl<-pl*len # size of matrix
      insert<-TRUE # used later
   }

   # load the library
   dyn.load("wood.so")

   ## code for running everything at once...
   wood_ret<-.C("wood_path",len=as.integer(len),start=as.integer(start), 
                x=as.double(xpoints),y=as.double(ypoints),
                nbnd=as.integer(nbnd),xbnd=as.double(xbnd), 
                ybnd=as.double(ybnd),pathlen=as.double(rep(0,pl)))

   # full MDS
   if(!insert){
      # get passed back an array which is the upper diagonal
      # create a matrix
      D<-matrix(0,len,len)
      # R fills columns first, so fill the lower triangle first
      # then take the transpose for the same effect   
      D[lower.tri(D)]<-wood_ret$pathlen
      D<-t(D)

      # transpose
      D<-D+t(D)

   # insertion
   }else{
      D<-matrix(wood_ret$pathlen,ncol=len,nrow=len-start)
      D<-t(D)
   }

   # load the library
   dyn.unload("wood.so")

   return(D)
}
