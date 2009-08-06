# wrapper function for C code
woodpath<-function(p1,p2,bnd){

   # put everything in the right format
   ap1<-c(p1$x,p1$y)
   ap2<-c(p2$x,p2$y)
   xbnd<-bnd$x
   ybnd<-bnd$y
   nbnd<-length(xbnd)

   # load the library
   dyn.load("wood.so")

   wood_ret<-.C("wood_path",p1=as.double(ap1), p2=as.double(ap2), nbnd=as.integer(nbnd),
                xbnd=as.double(xbnd), ybnd=as.double(ybnd), pathlen=as.double(0))


   return(wood_ret$pathlen)




}
