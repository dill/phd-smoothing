# plot the golf tee data from bookexamples

library(mrds)
data(book.tee.data)
pdf(file="ds-golftee.pdf",width=3,height=3)
par(mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

hist(book.tee.data$book.tee.dataframe$distance,main="",xlab="Distance (m)",breaks=c(seq(0,4,by=1/3)))
dev.off()

