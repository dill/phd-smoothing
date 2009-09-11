# wrapper function for C code
woodpath<-function(xpoints,ypoints,bnd){

   # put everything in the right format
   xbnd<-bnd$x
   ybnd<-bnd$y
   nbnd<-length(xbnd)
   len<-length(xpoints)

   # load the library
   dyn.load("wood.so")

   wood_ret<-.C("wood_path",len=as.integer(len),x=as.double(xpoints),y=as.double(ypoints),
                nbnd=as.integer(nbnd),
                xbnd=as.double(xbnd), ybnd=as.double(ybnd), pathlen=as.double(rep(0,(len^2-len)/2)))

   # get passed back an array which is the upper diagonal

   # create a matrix
   D<-matrix(0,len,len)

   # set the upper triangle to the values
   D[upper.tri(D)]<-wood_ret$pathlen
   
   # transpose
   D<-D+t(D)

   return(D)




}
