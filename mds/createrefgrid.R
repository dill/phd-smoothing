create_refgrid<-function(bnd){
   # create reference grid for the path finding

   # using a square grid: make_soap_grid   
#   grid<-make_soap_grid(bnd,8)


   # using triangulation

   library(ads)

   # first need to get rid of duplicate verts
   bndtmp<-pe(bnd,-1)

   bnd.tri<-triangulate(bndtmp)


   # centres:
   grid<-list(x=c(),y=c())
   grid$x<-(1/3)*rowSums(bnd.tri[,c(1,3,5)])
   grid$y<-(1/3)*rowSums(bnd.tri[,c(2,4,6)])



   return(grid)
}
