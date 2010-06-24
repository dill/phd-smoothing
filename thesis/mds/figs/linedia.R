# do a silly line diagram...


pdf(file="linedia.pdf",width=3,height=3)
par(mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex=0.7,cex.lab=0.7)

# plot area
plot(c(1.5,6.5),c(1.5,6.5),type="n",asp=1,xlab="x",axes=FALSE,ylab="")
axis(1)

### first line, a,b,c in right order, equally spaced
lines(c(1.5,6.5),c(6,6))
# ticks
lines(c(2,2),c(6.1,5.9))
lines(c(4,4),c(6.1,5.9))
lines(c(6,6),c(6.1,5.9))
# labels
text(c(2,4,6),rep(6.3,3),labels=c("a","b","c"))




# second line, squashed in order
lines(c(1.5,6.5),c(4,4))
# ticks
lines(c(3,3),c(4.1,3.9))
lines(c(3.5,3.5),c(4.1,3.9))
lines(c(6.5,6.5),c(4.1,3.9))
# labels
text(c(3,3.5,6.5),rep(4.3,3),labels=c("a","b","c"))


# third line out of order squash
lines(c(1.5,6.5),c(2,2))
# ticks
lines(c(1.5,1.5),c(2.1,1.9))
lines(c(5,5),c(2.1,1.9))
lines(c(6,6),c(2.1,1.9))
# labels
text(c(1.5,5,6),rep(2.3,3),labels=c("b","a","c"))




dev.off()
