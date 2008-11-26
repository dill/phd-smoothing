# Test to see what smoothing looks like


# Generate bivariate normal distribution
require(MASS)
bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), 2))
# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 10)


# fancy contour with image
#image(bivn.kde); contour(bivn.kde, add = T)

orig.data.set<-list(x=Re(my.grid),y=Im(my.grid),z=as.vector(bivn.kde$z))
orig.fit<-gam(z~s(x)+s(y)+s(x,y),data=orig.data.set)

morph.data.set<-list(x=Re(eval.points),y=Im(eval.points),z=as.vector(bivn.kde$z))
morph.fit<-gam(z~s(x)+s(y)+s(x,y),data=morph.data.set)

# Plot the two side by side
par(mfrow=c(1,2))
vis.gam(orig.fit)
vis.gam(morph.fit)




