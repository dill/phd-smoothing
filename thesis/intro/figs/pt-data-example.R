# example of pt data using the Amakihi data from Marques Auk paper

amakihi<-read.csv("amakihi.csv")
pdf(file="pt-data-example.pdf",height=3,width=3)
par(mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

hist(amakihi$distance[amakihi$distance<82.5],breaks=seq(0,85,by=5),xlab="Distance (m)",main="")
dev.off()
