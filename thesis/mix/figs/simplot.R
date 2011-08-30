# new sampling function

library(mmds)
plotcomp<-function(pars,mix.terms,pt=FALSE,asp=1){
   gp<-getpars(pars,mix.terms)
   sigmas<-gp$key.scale
   pis<-gp$mix.prop

   if(pt){
      intfcn<-integrate.hn.pt
   }else{
      intfcn<-integrate.hn
   }

   x<-seq(0,1,len=1000)
   pv<-matrix(0,mix.terms,length(x))
   mu<-c()

   for(i in 1:mix.terms){
      pv[i,]<-keyfct.hn(x,sigmas[i])
      mu<-c(mu,intfcn(sigmas[i],width))
   }
   ptotal<-detfct(x,pars,mix.terms)
   mutotal<-mu.calc(pars,mix.terms,width,pt=pt)

   if(pt){
      ptotal<-2*pi*x*ptotal/mutotal
      for(i in 1:mix.terms){
         pv[i,]<-2*pi*x*pv[i,]/mu[i]
      }
   }

   if(!pt){
      yl<-c(0,max(c(1,ptotal,pv)))
   }else{
      yl<-c(0,max(c(ptotal,pv)))
   }
   plot(x,ptotal,type="l",ylim=yl,xlim=c(0,1),
        asp=asp,axes=F)
   axis(1,c(0,0.5,1))
   if(!pt){
      axis(2,c(0,0.5,1))
   }else{
      axis(2)
   }
   box()
   for(i in 1:mix.terms){
      lines(x,pv[i,],lty=2)
   }
   # reference line for 15%
#   abline(h=0.15,col="red")
}


#postscript(file="sim-detfct.eps",width=9,height=9,
#            paper="special",horizontal=FALSE)
pdf(file="sim-detfct.pdf",width=9,height=9)

par(mfrow=c(4,4),mar=c(2,2.2,1.8,1.5),las=1)



### plot for lt and pt

width<-1
#mix.terms<-1
#pars<-c(0.5)
mix.terms<-2
parmat<-matrix(NA,4,3)
parmat[1,]<-c(log(0.8),log(0.15), inv.reparam.pi(0.3))
parmat[2,]<-c(log(0.6),log(0.1), inv.reparam.pi(1-0.3))
parmat[3,]<-c(log(10),log(0.2), inv.reparam.pi(0.15))
parmat[4,]<-c(log(0.7),log(0.05),inv.reparam.pi(0.6))

for(pt in c(FALSE,TRUE)){
   if(pt) asp<-NA else asp<-1
   for(par.ind in 1:4){
      pars<-parmat[par.ind,]  
      plotcomp(pars,mix.terms,pt,asp)
   }
}
   

### plot for 3-point
parmat<-matrix(NA,2,5)
parmat[1,]<-c(log(0.8),log(0.5),log(0.1),inv.reparam.pi(rep(1/3,3))[1:2])
parmat[2,]<-c(log(15),log(.25),log(0.05),inv.reparam.pi(c(0.1,0.4,0.5))[1:2])
mix.terms<-3

for(par.ind in 1:2){
   pars<-parmat[par.ind,]  
   plotcomp(pars,mix.terms,FALSE)
}

# two blank plots
plot(0,0,axes=F,type="n")
plot(0,0,axes=F,type="n")


# now plot the covariate models

# covsim1
model.formula<-"~as.factor(cov1)"
pars<-c(log(c(0.1,0.75,0.6)),inv.reparam.pi(0.4))
mix.terms<-2
n.samples<-1000
z<-list(matrix(c(rep(1,n.samples),rep(c(0,1),n.samples/2)),n.samples,2)) 
zdim<-2
testdata<-sim.mix(pars,mix.terms,n.samples,width,zdim,z)
names(testdata)[5]<-"cov1"
fit<-try(fitmix(testdata,mix.terms=2,ftype="hn",
            width=width,model.formula=model.formula))
plot(fit,nomf=T,hide.hist=T,style="comp",main=c(" "," "),x.axis=c(0,0.5,1))

# covsim2
model.formula<-"~cov1"
pars<-c(log(0.2),log(0.8),log(0.4), inv.reparam.pi(0.4))
z<-list(matrix(c(rep(1,n.samples),pnorm(seq(-4,4,len=n.samples))),n.samples,2))
zdim<-c(2)
testdata<-sim.mix(pars,mix.terms,n.samples,width,zdim,z)
names(testdata)[5]<-"cov1"
fit<-try(fitmix(testdata,mix.terms=2,ftype="hn",width=width,
                model.formula=model.formula))
plot(fit,nomf=T,hide.hist=T,style="comp",,main=c(" "," "),x.axis=c(0,0.5,1))


dev.off()
