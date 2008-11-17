
# need the routine inSide from soap
# http://www.maths.bath.ac.uk/~sw283/simon/software.html
require(mgcv)
require(soap)

# Setup the polygon

## Square
#polyvertices<-vector("complex",nvertices)
#polyvertices[1]<-complex(1,10,0)
#polyvertices[2]<-complex(1,0,10)
#polyvertices[3]<-complex(1,-10,0)
#polyvertices[4]<-complex(1,0,-10)
## Add extra vertex for going back to the start
#polyvertices[5]<-polyvertices[1]
#resolution<-0.5

## L-shape
#nvertices<-7
#polyvertices<-vector("complex",nvertices)
#polyvertices[1]<-complex(1,0,0)
#polyvertices[2]<-complex(1,2,0)
#polyvertices[3]<-complex(1,2,1)
#polyvertices[4]<-complex(1,1,1)
#polyvertices[5]<-complex(1,1,2)
#polyvertices[6]<-complex(1,0,2)
# Add extra vertex for going back to the start
#polyvertices[7]<-polyvertices[1]
#resolution<-0.4


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
par(mfrow=c(2,1),pch=".")

# Plot the original figure
plot(my.grid[inside.points],type="p")
lines(bnd)

# Calculate the new points
accuracy<-0.0001
eval.points<-sc.map.backwards(my.grid[inside.points],nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)

# Plot the new points
plot(eval.points)

