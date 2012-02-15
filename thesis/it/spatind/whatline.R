# diagnostic code
# creates a 2x2 plot of 
# * boxplot spatial residuals
# * boxplot temporal residuals
# * qqplot
# * scale-location plot

library(soap)

load("fullmod-Tweedie(1.2).RData") # load the data

# diagnostics - deviance residuals
#postscript(file="diag-deviance.eps",width=7,height=5)
#pdf(file="diag-deviance.eps",width=7)
#diagnostic(it.soap,res=5,resid.type="deviance")
model<-it.soap
res<-5
resid.type<-"deviance"

# boxplots are at resolution res
# resid.type residuals are used
# plot settings here
par(mfrow=c(1,1),pch=19,mar=c(3,3,2,1.5),cex.axis=0.7,mgp=c(1.75,1,0),cex.main=0.95)

### grab the residuals
resids<-residuals(model,type=resid.type)


### scale-location plot
sl.dat<-list(x=fitted(model),y=abs(residuals(model,type=resid.type)))
plot(sl.dat,las=1,
     main="Scale-location plot",
     ylab="Abs. value of residuals",
     xlab="Predicted values",cex=0.3)

ind<-fixdat$italy$dat$share_100==0
points(sl.dat$x[ind],sl.dat$y[ind],col="red",cex=0.3)


# loess fit..
#loe<-loess(y~x,data=sl.dat)
#nd<-seq(min(sl.dat$x,na.rm=T),max(sl.dat$x,na.rm=T))
#pred<-predict(loe,newdata=nd,by=0.01) 
#lines(nd,pred,col="grey")

