# aide memoir for boxplot code...

# original results
# soap comparison with sc+tp
soapcomp<-read.csv("results.file.txt",header=T)

# pspline fit
psplinecomp<-read.csv("pspline.results.txt",header=T)

# find the limits on the y axis
ylims<-c(0,max(soapcomp$mapped,soapcomp$soap,psplinecomp$x))

# plot the boxplots
par(mfrow=c(1,3))

# soap
boxplot(soapcomp$soap,ylim=ylims,main="soap")
# mapped+tp
boxplot(soapcomp$mapped,ylim=ylims,main="sc+tp")
# mapped+ps
boxplot(psplinecomp$mapped,ylim=ylims,main="sc+ps")
