# Code to make the "fig9" 2 panel diagram


fig9.true<-read.csv("../../fig9test/fig9truth.csv",header=TRUE)
fig9.poly<-read.csv("../../fig9test/figverts.csv",header=FALSE)

fig9.true$z[fig9.true$inside==0]<-NA

mat.dim<-sqrt(length(fig9.true$x))

pdf("fig9.pdf",4,4)

image(sort(unique(fig9.true$x)),sort(unique(fig9.true$y)),matrix(fig9.true$z,mat.dim,mat.dim),col=heat.colors(100),xlab="x",ylab="y",asp=1)
contour(sort(unique(fig9.true$x)),sort(unique(fig9.true$y)),matrix(fig9.true$z,mat.dim,mat.dim),add=TRUE)
lines(fig9.poly,lwd=3)

dev.off()
