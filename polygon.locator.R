# Script to use locator to draw polygons and then
# output the co-ordinates in the complex plane of 
# its vertices.

saved.par<-par()
par(mfrow=c(2,2))
# First, draw a plot window
plot.new()
# Add some axes
axis(1)
axis(2)
axis(3)
axis(4)


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
# Now re-centre about the centroid
points$x<-points$x-centroid$x
points$y<-points$y-centroid$y

# Find the complex representation of the vertices
points.complex<-paste(points$x,"+",points$y,"j",sep="")

# Pretty printer
cat("  'custom' : [", paste(points.complex,collapse=", "), "] }\n")

# Plot what the re-centred polygon looks like
plot(points$x,points$y,pch=".")
lines(c(points$x, points$x[1]),c(points$y,points$y[1]))


# Recover old par values
par(saved.par)
