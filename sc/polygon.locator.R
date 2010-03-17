# Subroutine to create a polygon based on points clicked with locator()
# Output:
#  vertices of the polygon, centered
#  the centroid
#  returns are complex

"polygon.locator"<-function(){

	# First, draw a plot window
	plot.new()
   # Make it a decent size
   plot.window( xlim =c(0,10),ylim=c(0,10))

	# Add some axes
	axis(1)
	axis(2)


   # sshhhh
   options(locatorBell=FALSE)

	# Now run locator, limit to 1000 (ie, n=999) sides, the last being the 
	# first point back to the last.
	points<-locator(n=999,type="l")
	
	# Add the last line in...
	lines(c(points$x[1],points$x[length(points$x)]) ,c(points$y[1],points$y[length(points$y)]))
	
	
	# We want to then translate the polygon to be centred at (0,0) and
	# order the points in a clockwise function.
	
	# First find the centroid
	centroid<-list()
	centroid$x<-1/length(points$x) * sum(points$x)
	centroid$y<-1/length(points$y) * sum(points$y)

	# Now re-centre the points, making these into complex
   #cpoints<-complex(length(points$x),points$x-centroid$x,points$y-centroid$y)
   # We re-centred on (0,0)
   #centre<-complex(1,0,0)


   # Without using the centering seems to work better
   cpoints<-complex(length(points$x),points$x,points$y)
   centre<-complex(1,centroid$x,centroid$y)
	 
   return(list(vertices=cpoints,centre=centre))

}
