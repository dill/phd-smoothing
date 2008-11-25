
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


# quick hack
polyvertices.save<-polyvertices
polyvertices<-polyvertices[1:nvertices]

# Calculate the new points
accuracy<-0.0001
res<-sc.map.backwards(my.grid[inside.points],nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)

eval.points<-res$eval.points

# find the points that produced errors
flagged.points<-res$errors
# colour them red on the original space
points(Re(flagged.points),Im(flagged.points),pch=19,col="red")

# Number the vertices
text(x=Re(polyvertices),y=Im(polyvertices),labels=seq(1,nvertices,1),adj=1.5,col="blue")

# Put in the diagonals
diag.1<-complex(length(seq(5,0,-0.05)),seq(5,0,-0.05),seq(5,0,-0.05))
diag.2<-complex(length(seq(5,0,-0.05)),seq(5,0,-0.05),seq(0,5,0.05))
lines(diag.1,col="green")
lines(diag.2,col="green")


# Plot the new points
plot(eval.points,xlab="x",ylab="y")#,main="Transformed domain")


polyvertices<-polyvertices.save


# Now take the vertices of the polygon and put them on the plot
ret.vertices<-sc.map.backwards(polyvertices,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)

ret.vertices<-ret.vertices$eval.points

text(x=Re(ret.vertices),y=Im(ret.vertices),labels=seq(1,nvertices,1),adj=1.5,col="blue")



# And the same with the diagonals
ret.diag.1<-sc.map.backwards(diag.1,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)
ret.diag.1<-ret.diag.1$eval.points
lines(ret.diag.1,col="green")

ret.diag.2<-sc.map.backwards(diag.2,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)
ret.diag.2<-ret.diag.2$eval.points
lines(ret.diag.2,col="green")

