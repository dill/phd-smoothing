# diagram to show that the shortest path is 
# unique in a simple polygon

pdf(file="unique-path-dia.pdf",5,5)


# create the polygon
poly<-list(x=c(1,2,4,5,4,3.5,3,2.5,2,1),
           y=c(1,2,2,1,0,0.5,0,0.5,0,1))

plot(poly,type="l",xlab="",ylab="",las=1)

# p1 and p2 -- the start and end points
p1<-list(x=2,y=0.5)
p2<-list(x=4,y=0.5)



# fist path between p1 and p2
path1.1<-list(x=c(p1$x,2.5),y=c(p1$y,p1$y))
path1.2<-list(x=c(3.5,p2$x),y=c(p1$y,p1$y))
lines(path1.1,col="red")
lines(path1.2,col="red")
# dotted line in the middle
loop1<-list(x=c(2.5,3.5),y=c(0.5,0.5))
lines(loop1,col="red",lty=2)

# second path
xvals<-seq(2.5,3.5,len=10)
path2<-list(x=xvals,y=1.37-sqrt(1-(xvals-3)^2))
path1.1$y<-path1.1$y+0.008
path1.2$y<-path1.2$y+0.008
lines(path1.1,col="blue")
lines(path1.2,col="blue")
lines(path2,col="blue",lty=2)


# put the points in (so the dots go over the line ends)

# p1 and p2
points(p1,pch=19)
text(p1,label=expression(p[1]),pos=2)
points(p2,pch=19)
text(p2,label=expression(p[2]),pos=4)

# v1 and v2 -- the points where the paths diverge
v1<-list(x=2.5,y=0.5)
v2<-list(x=3.5,y=0.5)
points(v1,pch=18)
text(v1,label=expression(v[1]^"*"),pos=3)
points(v2,pch=18)
text(v2,label=expression(v[2]^"*"),pos=3)

dev.off()
