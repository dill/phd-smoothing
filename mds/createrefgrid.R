create_refgrid<-function(bnd){
   # create reference grid for the path finding

   # using a square grid: make_soap_grid   
   grid1<-make_soap_grid(bnd,10)


   # using triangulation

   library(ads)

   # first need to get rid of duplicate verts
   bndtmp<-pe(bnd,-1)

   bnd.tri<-triangulate(bndtmp)


   # centres:
   grid2<-list(x=c(),y=c())
   grid2$x<-(1/3)*rowSums(bnd.tri[,c(1,3,5)])
   grid2$y<-(1/3)*rowSums(bnd.tri[,c(2,4,6)])

   grid<-list(x=c(grid1$x,grid2$x),y=c(grid1$y,grid2$y))
   

   return(grid)
}
