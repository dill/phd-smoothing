source("make.grid.R")

# Call the routine to create the grid
my.grid<-make.grid(polyvertices,resolution)


# Set plotting parameters
par(mfrow=c(1,2))

# Plot the original figure
plot(my.grid,xlab="",ylab="",pch=".")
lines(polyvertices)
lines(c(polyvertices[1],polyvertices[length(polyvertices)]))
#points(my.grid,col="blue",pch=21)

# Calculate the new points
accuracy<-0.0001
res<-sc.map.backwards(my.grid,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)

eval.points<-res$eval.points

# find the points that produced errors
flagged.points<-my.grid[res$errors]
# colour them red on the original space
points(Re(flagged.points),Im(flagged.points),pch=19,col="red")

# Number the vertices
text(x=Re(polyvertices),y=Im(polyvertices),labels=seq(1,nvertices,1),adj=1.5,col="blue")

# plot the centre point
points(Re(wc),Im(wc),pch=18,col="blue")
text(x=Re(wc),y=Im(wc),labels="wc",adj=1.5,col="blue")


# Put in the diagonals
#diag.1<-complex(length(seq(5,0,-0.05)),seq(5,0,-0.05),seq(5,0,-0.05))
#diag.2<-complex(length(seq(5,0,-0.05)),seq(5,0,-0.05),seq(0,5,0.05))
#lines(diag.1,col="green")
#lines(diag.2,col="green")


# Plot the new points
plot(eval.points,xlab="",ylab="",pch=".")

# Now take the vertices of the polygon and put them on the plot
#ret.vertices<-sc.map.backwards(polyvertices,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)

#mapped.vertices<-ret.vertices$eval.points

#text(x=Re(mapped.vertices),y=Im(mapped.vertices),labels=seq(1,nvertices,1)[!ret.vertices$errors],adj=1.5,col="blue")



# And the same with the diagonals
#ret.diag.1<-sc.map.backwards(diag.1,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)
#ret.diag.1<-ret.diag.1$eval.points
#lines(ret.diag.1,col="green")
#
#ret.diag.2<-sc.map.backwards(diag.2,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)
#ret.diag.2<-ret.diag.2$eval.points
#lines(ret.diag.2,col="green")
#
