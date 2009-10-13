# wrapper function for C code
woodpath<-function(xpoints,ypoints,bnd,start,len){

   # put everything in the right format
   xbnd<-bnd$x
   ybnd<-bnd$y
   nbnd<-length(xbnd)

   if(is.na(len)){
      len<-length(xpoints)
   }

   # create a matrix
   D<-matrix(0,len,len)

   # load the library
   dyn.load("wood.so")

   ## code for running everything at once...
   wood_ret<-.C("wood_path",len=as.integer(len),start=as.integer(start), 
                x=as.double(xpoints),y=as.double(ypoints),
                nbnd=as.integer(nbnd),xbnd=as.double(xbnd), 
                ybnd=as.double(ybnd),pathlen=as.double(rep(0,(len*(len-1)/2))))
   # get passed back an array which is the upper diagonal

   # R fills columns first, so fill the lower triangle first
   # then take the transpose for the same effect   
   D[lower.tri(D)]<-wood_ret$pathlen

   D<-t(D)


   # load the library
   dyn.unload("wood.so")

   # transpose
   D<-D+t(D)

   return(D)
}
