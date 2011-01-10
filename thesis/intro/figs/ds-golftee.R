# plot the golf tee data from bookexamples

library(mrds)
data(book.tee.data)
pdf(file="ds-golftee.pdf",width=4,height=4)
hist(book.tee.data$book.tee.dataframe$distance,main="",xlab="Distance (m)",breaks=c(seq(0,4,by=1/3)))
dev.off()

