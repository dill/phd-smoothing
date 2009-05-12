# make some boxplots

pdf(file="mses-boxplot.pdf",width=4,height=4)

# make the text look better for printout
par(cex.lab=0.75)

# b l t r
par(mar=c(7,3,2,1)+.1)
par(mgp=c(6,2,0))

pspl<-read.csv("pspline.results.txt")
other<-read.csv("results.file.txt")


dat<-list(labs=c(rep("P-splines",length(pspl$mapped)),
                 rep("TPRS",length(other$mapped)),
                 rep("soap",length(other$soap))),
          mse=c(pspl$mapped,other$mapped,other$soap))


mses<-c(signif(mean(dat$mse[dat$labs=="P-splines"],na.rm=TRUE),3),
        signif(mean(dat$mse[dat$labs=="TPRS"],na.rm=TRUE),3),
        signif(mean(dat$mse[dat$labs=="soap"],na.rm=TRUE),3))

ses<-c(signif(sd(dat$mse[dat$labs=="P-splines"],na.rm=TRUE),3),
        signif(sd(dat$mse[dat$labs=="TPRS"],na.rm=TRUE),3),
        signif(sd(dat$mse[dat$labs=="soap"],na.rm=TRUE),3))

xlab=paste("P-splines: MSE=",mses[1],"se(MSE)=",ses[1],"\n",
           "TPRS: MSE=",mses[2],"se(MSE)=",ses[2],"\n",
           "soap: MSE=",mses[3],"se(MSE)=",ses[3],"\n")

boxplot(mse~labs,data=dat,main="",xlab=xlab)

dev.off()
