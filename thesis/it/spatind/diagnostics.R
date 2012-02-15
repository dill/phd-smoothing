# diagnostic code
# creates a 2x2 plot of 
# * boxplot spatial residuals
# * boxplot temporal residuals
# * qqplot
# * scale-location plot

# boxplots are at resolution res
# resid.type residuals are used
#diagnostic<-function(model,res=20,resid.type="deviance"){
   # plot settings here
   par(mfrow=c(3,2),pch=19,mar=c(3,3,2,1.5),cex.axis=0.7,mgp=c(1.75,1,0),cex.main=0.95)

   ### grab the residuals
   resids<-residuals(model,type=resid.type)
   
   ### year data
   yeardata<-data.frame(ind=it.dat$year,resids=resids)
   boxplot(resids~ind,data=yeardata,main="Annual distribution of residuals",
            cex=0.3,las=1,xaxt="n",
            xlab="year",ylab="Residuals")
   axis(1,at=1:6,labels=c("03","04","05","06","07","08"))

   # skip a row
   plot.new()

   ### spatial boxplot and reference
   # first generate the grid...
   it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)
   xmax<-max(it$x)
   ymax<-max(it$y)
   xmin<-min(it$x)
   ymin<-min(it$y)
   delx<-(xmax-xmin)/res
   dely<-(ymax-ymin)/res
   
   xseq<-seq(xmin,xmax,delx)
   yseq<-seq(ymin,ymax,dely)

   xm<-xseq[1:(length(xseq)-1)]+delx/2
   ym<-yseq[1:(length(yseq)-1)]+dely/2
   mgrid<-expand.grid(x=xm,y=ym)

   # find the grid cells the residuals lie in...
   xi<-abs(floor((it.dat$x-xmin)/delx))#+1
   yj<-abs(floor((it.dat$y-ymin)/dely))#+1

   boxi<-(yj)*res+(xi+1)
   labs<-rep("",25)
   labs[sort(unique(boxi))]<-1:length(unique(boxi))

   
   ### box index...
   boxind<-data.frame(ind=boxi,resids=resids)
   boxplot(resids~ind,data=boxind,main="Spatial aggregation index",
            xlab="Index",ylab="Residuals",
            cex=0.3,xaxt="n",las=1)
   axis(1,at=1:length(unique(boxind$ind)))

   # plot the aggregations
   plot(it,asp=1,type="l",xlab="km (e)",ylab="km (n)",
            main="Spatial aggregation reference",col="grey")#,cex.main=1.4,
            #cex.lab=1.4,cex.axis=1.3)
   for(i in 1:length(xseq)){
      lines(x=c(xseq[i],xseq[i]),y=c(ymin,ymax))#,col="grey")
      for(j in 1:length(yseq)){
         lines(x=c(xmin,xmax),y=c(yseq[j],yseq[j]))#,col="grey")
      }
   }
   text(mgrid,labels=labs,cex=0.7)


   ### Normal qqplot
   qqnorm(residuals(model,type=resid.type),cex=0.3,asp=1,las=1)
   abline(0,1)

   ### scale-location plot
   sl.dat<-list(x=fitted(model),y=abs(residuals(model,type=resid.type)))
   plot(sl.dat,las=1,
        main="Scale-location plot",
        ylab="Abs. value of residuals",
        xlab="Predicted values",cex=0.3)

   # loess fit..
   loe<-loess(y~x,data=sl.dat)
   nd<-seq(min(sl.dat$x,na.rm=T),max(sl.dat$x,na.rm=T))
   pred<-predict(loe,newdata=nd,by=0.01) 
   lines(nd,pred,col="grey")

   # here are some zeros
   ind<-fixdat$italy$dat$share_100==0
   points(sl.dat$x[ind],sl.dat$y[ind],col="grey",cex=0.3)


#}
