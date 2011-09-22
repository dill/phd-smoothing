# thin plate example

pdf(file="tprsex.pdf",width=5,height=5)

# plotting options
par(mfrow=c(2,2),mar=c(2,2.2,1.8,1.5),las=1)

# grid setup 
n<-100
x1<-seq(0,1,len=n)
x.grid<-expand.grid(x1,x1)

cols<-heat.colors(1000)

# constant
image(matrix(1,n,n),col=cols,zlim=c(0,1),xlab=expression(x[1]),
                                    ylab=expression(x[2]),asp=1)
contour(matrix(1,n,n),add=TRUE)

# phi_2=x_1
image(matrix(x1,n,n),col=cols,zlim=c(0,1),xlab=expression(x[1]),
                                    ylab=expression(x[2]),asp=1)
contour(matrix(x1,n,n),add=TRUE)

image(t(matrix(x1,n,n)),col=cols,zlim=c(0,1),xlab=expression(x[1]),
                                    ylab=expression(x[2]),asp=1)
contour(t(matrix(x1,n,n)),add=TRUE)

# radial basis function
rbf<-function(x1,x2){

   r<-sqrt(rowSums((x1-x2)^2))

   1/(8*pi) * r^2 * log(r)
}
res<- -100*rbf(x.grid,c(0.5,0.5))
img.mat<-matrix(res,n,n)
image(img.mat,col=cols,zlim=c(0,1),xlab=expression(x[1]),
                                    ylab=expression(x[2]),asp=1)
contour(img.mat,add=TRUE)

dev.off()
