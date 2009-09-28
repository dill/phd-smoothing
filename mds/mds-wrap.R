# wrapper function for C code
woodpath<-function(xpoints,ypoints,bnd){

   # put everything in the right format
   xbnd<-bnd$x
   ybnd<-bnd$y
   nbnd<-length(xbnd)
   len<-length(xpoints)

   # create a matrix
   D<-matrix(0,len,len)

   # load the library
   dyn.load("wood.so")



   for(i in 1:len){
      for(j in i:len){

         xp<-c(xpoints[i],xpoints[j])
         yp<-c(ypoints[i],ypoints[j])

         wood_ret<-.C("wood_path",len=as.integer(2),x=as.double(xp),y=as.double(yp),
                nbnd=as.integer(nbnd),
                xbnd=as.double(xbnd), ybnd=as.double(ybnd), pathlen=as.double(0))

#         cat(wood_ret$pathlen,"\n")

         D[i,j]<-wood_ret$pathlen


      }
   }



   # get passed back an array which is the upper diagonal
   # set the upper triangle to the values
#   D[upper.tri(D)]<-wood_ret$pathlen
   
   # transpose
   D<-D+t(D)

   return(D)
}
