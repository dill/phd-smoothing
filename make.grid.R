# Function to create a grid over a polygon


make.grid<-function(polyvertices, resolution){
   # Args
   #  polyvertices   complex vector of vertices of the polygon
   #  resolution     resolution of the grid

   # check to see if we have the last=first vertex
   if(polyvertices[length(polyvertices)]!=polyvertices[1]){
      polyvertices[nvertices+1]<-polyvertices[1]
   }


   # need the routine inSide from soap
   # http://www.maths.bath.ac.uk/~sw283/simon/software.html
   require(mgcv)
   require(soap)

   # Set the boundary
	bnd<-list()
	bnd$x<-Re(polyvertices)
	bnd$y<-Im(polyvertices)
	
	#Create the grid
	# This is kind of inefficient at the moment
	gridd<-seq(min(c(bnd$x,bnd$y)),max(c(bnd$x,bnd$y)),resolution)
	my.grid<-c()
	it<-1
	for (i in gridd){
	   for (j in gridd){
	      my.grid[it]<-complex(1,i,j)
	      it<-it+1
	   }
	}
	
	# Find the points that are inside the boundary
	inside.points<-inSide(bnd,Re(my.grid),Im(my.grid))


   # Return
   return(my.grid[inside.points])

}
