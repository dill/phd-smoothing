
# need the routine inSide from soap
# http://www.maths.bath.ac.uk/~sw283/simon/software.html
require(mgcv)
require(soap)

# set the bounds for the grid program
real.points<-c()
imag.points<-c()

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

# Set plotting parameters
par(mfrow=c(1,2),pch=".")

# Plot the original figure
plot(my.grid[inside.points],xlab="x",ylab="y")#,main="Original domain")
lines(bnd)

# Calculate the new points
accuracy<-0.0001
eval.points<-sc.map.backwards(my.grid[inside.points],nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)

# Plot the new points
plot(eval.points,xlab="x",ylab="y")#,main="Transformed domain")

