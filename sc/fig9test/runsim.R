#  simulation experiment
# This is run from runsims

cat("Simulation starts!\n")
cat("Samples per iteration:",samp.size,"\n")
cat("Noise level:",noise.level,"\n")

# vars to hold the mses
sctprs.mse.disk<-c()
sctprs.mse.rect<-c()
tprs.mse<-c()
soap.mse<-c()

# do 500 replicates
for (i in 1:500){

   # make a sample index
   this.sample<-sample(c(1:dim(true.vals)[1]),samp.size)
   
   # noise
   ran<-rnorm(samp.size)*noise.level
   
   # take the points from the true and true mapped
   samp.data<-data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1],z=true.vals$z[true.vals$inside==1])
   samp.data<-data.frame(x=samp.data$x[this.sample],y=samp.data$y[this.sample],z=samp.data$z[this.sample]+ran)
   samp.data.mapped.disk<-data.frame(x=true.vals.mapped.disk$x[this.sample],y=true.vals.mapped.disk$y[this.sample],z=true.vals.mapped.disk$z[this.sample]+ran)
   samp.data.mapped.rect<-data.frame(x=true.vals.mapped.rect$x[this.sample],y=true.vals.mapped.rect$y[this.sample],z=true.vals.mapped.rect$z[this.sample]+ran)
   
   
   ### disk mapping
   b.mapped.disk<-gam(z~s(x,y,k=49),data=samp.data.mapped.disk)
   fv.disk <- predict(b.mapped.disk,newdata=data.frame(x=true.vals.mapped.disk$x,y=true.vals.mapped.disk$y))
   
   ### rect mapping
   b.mapped.rect<-gam(z~s(x,y,k=49),data=samp.data.mapped.rect)
   fv.rect <- predict(b.mapped.rect,newdata=data.frame(x=true.vals.mapped.rect$x,y=true.vals.mapped.rect$y))

   ### normal tprs
   b.tprs<-gam(z~s(x,y,k=49),data=samp.data)
   fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))
   
   ### soap fit
   b.soap<-gam(z~s(x,y,bs="so",xt=list(bnd=list(verts)),k=soap.boundary.knots),data=samp.data,knots=knots)
   fv.soap <- predict(b.soap,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))
   
   ### calculate the MSEs
   sctprs.mse.disk<-c(sctprs.mse.disk,mean((true.vals$z[true.vals$inside==1]-fv.disk)^2,na.rm=T))
   sctprs.mse.rect<-c(sctprs.mse.rect,mean((true.vals$z[true.vals$inside==1]-fv.rect)^2,na.rm=T))
   tprs.mse<-c(tprs.mse,mean((true.vals$z[true.vals$inside==1]-fv.tprs)^2,na.rm=T))
   soap.mse<-c(soap.mse,mean((true.vals$z[true.vals$inside==1]-fv.soap)^2,na.rm=T))
   
}

mses<-c(sctprs.mse.disk,sctprs.mse.rect,tprs.mse,soap.mse)
labs<-c(rep("SC+TPRS\n(disk)",length(sctprs.mse.disk)),rep("SC+TPRS\n(rectangle)",length(sctprs.mse.rect)),rep("TPRS",length(tprs.mse)),rep("soap",length(soap.mse)))

mse.data<-list(mse=mses,labs=labs)

cat("writing to file:",paste("mses-",samp.size,"-",noise.level,".txt",sep=""))
write.csv(mse.data,file=paste("mses-",samp.size,"-",noise.level,".txt",sep=""))

cat("data written!\n")
 
cat("simulation done!\n")

#sink(NULL)
