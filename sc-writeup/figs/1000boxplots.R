# comparison of the 1000,0.3 results for the horseshoe

# load the data
psp<-read.csv(paste("../../ramseysim/pspline.results.txt",sep=""))
soap<-read.csv(paste("../../ramseysim/results.file.txt",sep=""))

pdf("1000boxplots.pdf",4,4)
par(mfrow=c(1,2))

# limits
ylims<-c(0,max(soapcomp$mapped,soapcomp$soap,psplinecomp$x))

# plot the boxplots
# mapped+ps
boxplot(psplinecomp$mapped,ylim=ylims,main="",pch=20,cex=0.5,cex.axis=0.5)
# soap
boxplot(soapcomp$soap,ylim=ylims,main="",pch=20,cex=0.5,cex.axis=0.5)

dev.off()


