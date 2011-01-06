# diagram to illustrate the reparameterisation of the pis

library(mmds)

pis<-c(0.1,0.2,0.3,0.4)
alphas<-inv.reparam.pi(pis,lastpar=TRUE)

par(mgp=c(1,1,0),mar=c(3,3,1,1))

xend<-4
x<-seq(0,xend,len=1000)

plot(x=seq(0,xend,len=3),y=seq(0,1,len=3),type="n",axes=FALSE,
      ylab=expression(F(e^alpha)),xlab=expression(e^alpha))
lines(x,pgamma(x,3,2))

expalpha<-cumsum(exp(alphas))
Falpha<-pgamma(cumsum(exp(alphas)),3,2)
for( i in 1:(length(pis)-1)){
   # pi_i
   lines(x=rep(expalpha[i],2),y=c(0,Falpha[i]),lty=2)
   lines(x=c(0,expalpha[i]),y=rep(Falpha[i],2),lty=2)
}

# draw the axes
axis(1,at=c(0,expalpha,2,3,4),labels=c(0,round(expalpha,2),2,3,4),pos=0)
axis(2,at=c(0,Falpha,1),labels=c(0,Falpha,1),pos=0)

# put the lines on that show the pis
lines(x=rep(0.25,2),y=c(0,Falpha[1]),lwd=2)
points(x=0.25,y=Falpha[1]-0.007,pch=24,cex=0.7,bg="black")
points(x=0.25,y=0+0.007,pch=25,cex=0.7,bg="black")
text(x=0.1,y=Falpha[1]/2,lab=expression(pi[1]))


lines(x=rep(0.25,2)+0.1,y=c(Falpha[1],Falpha[2]),lwd=2)
points(x=0.25+0.1,y=Falpha[2]-0.005,pch=24,cex=0.7,bg="black")
points(x=0.25+0.1,y=Falpha[1]+0.007,pch=25,cex=0.7,bg="black")
text(x=0.2,y=(Falpha[1]+Falpha[2])/2,lab=expression(pi[2]))

lines(x=rep(0.25,2)+0.2,y=c(Falpha[2],Falpha[3]),lwd=2)
points(x=0.25+0.2,y=Falpha[3]-0.005,pch=24,cex=0.7,bg="black")
points(x=0.25+0.2,y=Falpha[2]+0.007,pch=25,cex=0.7,bg="black")
text(x=0.3,y=(Falpha[2]+Falpha[3])/2,lab=expression(pi[3]))

lines(x=rep(0.25,2)+0.3,y=c(Falpha[3],Falpha[4]),lwd=2)
points(x=0.25+0.3,y=Falpha[4]-0.005,pch=24,cex=0.7,bg="black")
points(x=0.25+0.3,y=Falpha[3]+0.007,pch=25,cex=0.7,bg="black")
text(x=0.4,y=(Falpha[3]+Falpha[4])/2,lab=expression(pi[4]))


