# examples of detection functions

# x data
x<-seq(0,1,len=100)

# half normal
keyfct.hn<-function(distance, key.scale){
   exp( - (( distance/ (sqrt(2) * key.scale) )^2) )
}

keyfct.hz<-function(distance, key.scale, key.shape){
   1 - exp( - (distance/key.scale)^( - key.shape))
}



pdf(file="detfct-examples.pdf",width=6,height=2)

par(mfrow=c(1,3),mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

plot(x=x,y=keyfct.hn(x,0.3),type="l",xlab="Distance",ylab="Detection probability")
plot(x=x,y=keyfct.hz(x,0.3,5),type="l",xlab="Distance",ylab="Detection probability")
plot(x=x,y=keyfct.hn(x,0.3)*(1+0.7*cos(2*pi*x)),type="l",xlab="Distance",ylab="Detection probability")

dev.off()
