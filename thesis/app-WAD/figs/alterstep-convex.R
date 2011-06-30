# alter step simplest case


chevron.top<-list(x=c(-1,0,1),
                  y=c(0,1,0))
chevron.bot<-list(x=c(-1,-0.5,-0.4,-0.4,-0.2,0,0.2,0.4,0.4,0.5,1),
                  y=c(-1,-0.25,-0.25,-0.1,-0.1,0.15,-0.1,-0.1,-0.25,-0.25,-1))


plot(c(-1,1),c(1.1,-1),type="n",asp=1,xlab="",ylab="",axes=F)

# draw the chevrons and the grey bit
polygon(chevron.bot,col=grey(0.8),border=NA)
lines(chevron.top,lwd=1.5)
lines(chevron.bot,lwd=1.5)

v1<-list(x=-0.75,y=-0.5)
v2<-list(x=0,y=1)
v3<-list(x=0.75,y=-0.5)

v1s<-list(x=0,y=0)

# the bad path line
bad.path<-list(x=c(v1$x,v2$x,v3$x),
               y=c(v1$y,v2$y,v3$y))
lines(bad.path,lty=2)


# good path
good.path<-list(x=c(v1$x,-0.4,0,0.4,v3$x),
                y=c(v1$y,-0.1,0.15,-0.1,v3$y))
lines(good.path,lty=2,col="blue")



# the points v1, v2 and v3
points(v1,pch=19,cex=0.7)
points(v2,pch=19,cex=0.7)
points(v3,pch=19,cex=0.7)
text(v1,label=expression(v[i]),pos=1)
text(v2,label=expression(v[i+1]),pos=3)
text(v3,label=expression(v[i+2]),pos=1)


#points(v1s,pch=19,cex=0.7)
#text(v1s,label=expression(v[i+1]^"*"),pos=3)

