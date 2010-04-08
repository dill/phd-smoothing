create_refgrid<-function(bnd,dens=20){
   # create reference grid for the path finding

   # starting value
   res<-10

   # using a square grid: make_soap_grid   
   grid<-make_soap_grid(bnd,res,log=TRUE,delta=TRUE)
   
   while(length(grid$x)<dens){
      res<-res+1
      grid<-make_soap_grid(bnd,res)
   }

   grid$nrefx<-res
   grid$nrefy<-res

   return(grid)
}
