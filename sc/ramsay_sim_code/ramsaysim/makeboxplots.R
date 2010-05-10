# make some boxplots

#pdf(file="mses-boxplot.pdf",width=4,height=4)

# make the text look better for printout
par(cex.lab=0.75)

# b l t r
#par(mar=c(7,3,2,1)+.1)
#par(mgp=c(6,2,0))
par(las=1)

pspl<-read.csv("pspline.results.txt")
other<-read.csv("results.file.txt")
tprs<-read.csv("../tp-0.3-1000.results.txt")

dat<-list(labs=c(rep("sc + p-splines",length(pspl$mapped)),
                 rep("sc + tprs",length(other$mapped)),
                 rep("soap",length(other$soap)),
                 rep("tprs",length(tprs$tp))),
          mse=c(pspl$mapped,other$mapped,other$soap,tprs$tp))


mses<-c(signif(mean(dat$mse[dat$labs=="sc + p-splines"],na.rm=TRUE),3),
        signif(mean(dat$mse[dat$labs=="sc + tprs"],na.rm=TRUE),3),
        signif(mean(dat$mse[dat$labs=="soap"],na.rm=TRUE),3),
        signif(mean(dat$mse[dat$labs=="tprs"],na.rm=TRUE),3))

ses<-c(signif(sd(dat$mse[dat$labs=="sc + p-splines"],na.rm=TRUE),3),
        signif(sd(dat$mse[dat$labs=="sc + tprs"],na.rm=TRUE),3),
        signif(sd(dat$mse[dat$labs=="soap"],na.rm=TRUE),3),
        signif(sd(dat$mse[dat$labs=="tprs"],na.rm=TRUE),3))

xlab=paste("sc + p-splines: MSE=",mses[1]," se(MSE)=",ses[1],"\n",
           "sc + tprs: MSE=",mses[2]," se(MSE)=",ses[2],"\n",
           "soap: MSE=",mses[3]," se(MSE)=",ses[3],"\n",
           "tprs: MSE=",mses[4]," se(MSE)=",ses[4],"\n",sep="")


# log for plotting
dat$mse<-log(dat$mse)

boxplot(mse~labs,data=dat,main="",axes=FALSE)#,xlab=xlab)

axis(1,at=c(0,1,2,3,4),label=c("",unique(dat$labs)),tick=FALSE,line=NULL)
axis(2,at=seq(-7,0,by=1),label=seq(-7,0,by=1))

legend(0.25,-2,xlab,bty="n")

#dev.off()
