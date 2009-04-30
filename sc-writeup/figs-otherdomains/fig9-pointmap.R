# Plot the point mapping for fig9


domain<-c("disk","rect")
vert.pos<-list(c(rep(3,5),4,2,2,4),c(rep(4,6),3,2,2))

# for testing
#par(mfrow=c(1,2))

for(i in c(1:2)){


   # load the values
   true.vals.mapped<-read.csv(paste("../../fig9test/fig9truemapped-",domain[i],".csv",sep=""),header=FALSE)
   names(true.vals.mapped)<-c("x","y","z")
   
   
   # load the vertices data
   verts<-read.csv(paste("../../fig9test/mappedfigverts-",domain[i],".csv",sep=""),header=FALSE)
   names(verts)<-c("x","y")
   
   # do some plotting
   pdf(file=paste("fig9-pointplot-",domain[i],".pdf"))
   plot(true.vals.mapped$x,true.vals.mapped$y,type="p",pch=19,asp=1,cex=0.25,main="",xlab="x",ylab="y")
   text(verts$x[1:9],verts$y[1:9],labels=c(1:length(vert.pos[[i]])),col="blue",pos=vert.pos[[i]])
   dev.off()

}


