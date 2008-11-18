# Subroutine to create a polygon based on points clicked with locator()
# Output:
#  vertices of the polygon, centered
#  the centroid
#  returns are complex

"polygon.locator"<-function(){

	saved.par<-par()
	par(mfrow=c(2,1))
	# First, draw a plot window
	plot.new()
   # Make it a decent size
   plot.window( xlim =c(-10,10),ylim=c(-10,10))

	# Add some axes
	axis(1)
	axis(2)


   # sshhhh
   options(locatorBell=FALSE)

	# Now run locator, limit to 10 (ie, n=9) sides, the last being the 
	# first point back to the last.
	points<-locator(n=9,type="l")
	
	# Add the last line in...
	lines(c(points$x[1],points$x[length(points$x)]) ,c(points$y[1],points$y[length(points$y)]))
	
	
	# We want to then translate the polygon to be centred at (0,0) and
	# order the points in a clockwise function.
	
	# First find the centroid
	centroid<-list()
	centroid$x<-1/length(points$x) * sum(points$x)
	centroid$y<-1/length(points$y) * sum(points$y)
	# Now re-centre the points
	points$x<-points$x-centroid$x
	points$y<-points$y-centroid$y
	
	# Plot what the re-centred polygon looks like
	plot(points$x,points$y,pch=".")
	lines(c(points$x, points$x[1]),c(points$y,points$y[1]))
	 
   # Making these into complex
   cpoints<-complex(length(points$x),points$x,points$y)
   # We re-centred on (0,0)
   centre<-complex(1,0,0)
	 
	# Recover old par values
	par(saved.par)

   return(list(vertices=cpoints,centre=centre))

}
