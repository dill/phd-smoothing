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
#points(Re(flagged.points),Im(flagged.points),pch=19,col="red")

# Number the vertices
text(x=Re(polyvertices),y=Im(polyvertices),labels=seq(1,nvertices,1),adj=1.5,col="blue")

# plot the centre point
points(Re(wc),Im(wc),pch=18,col="blue")
text(x=Re(wc),y=Im(wc),labels="wc",adj=1.5,col="blue")


# Plot the new points
plot(eval.points,xlab="",ylab="",pch=".")

# Now take the vertices of the polygon and put them on the plot
text(x=Re(prevertices),y=Im(prevertices),col="blue",labels=as.character(1:length(prevertices)))

