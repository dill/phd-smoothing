create_refgrid<-function(bnd){
   # create reference grid for the path finding

   
   grid<-make_soap_grid(bnd,10)


   return(grid)
}
