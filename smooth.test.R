# Test to see what smoothing looks like



# Generate bivariate normal distribution
require(MASS)
bivn <- mvrnorm(2500, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), 2))
# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n =50)


# fancy contour with image
#image(bivn.kde); contour(bivn.kde, add = T)


x<-c();y<-c()
for (i in 1:length(bivn.kde$x)){
   x<-c(x,rep(bivn.kde$x[i],50))
}
y<-rep(bivn.kde$y,50)


orig.data.set<-list(x=as.vector(x),y=as.vector(y),z=as.vector(bivn.kde$z))
orig.fit<-gam(z~s(x,y,bs="tp"),data=orig.data.set)


morph.data.set<-list(x=Re(eval.points),y=Im(eval.points),z=as.vector(bivn.kde$z[!res$errors]))
morph.fit<-gam(z~s(x,y,bs="tp"),data=morph.data.set)


par(mfrow=c(1,2))

vis.gam(orig.fit,plot.type="contour")
vis.gam(morph.fit,plot.type="contour")




