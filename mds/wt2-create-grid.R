wt2_create_grid<-function(res=74){
   ## create a boundary...
   bnd <- read.csv("wt2-verts.csv",header=FALSE)
   names(bnd)<-c("x","y")
   
   ## Simulate some fitting data, inside boundary...
   #gendata <- read.csv("wt2truth.csv",header=TRUE)
   gendata<-list(x=c(),y=c(),inside=c())
   m<-res;n<-res
   xm <- seq(-3,3.5,length=m);yn<-seq(-3,3,length=n)
   xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
   onoff<-inSide(bnd,xx,yy)
   onoff2<-inSide(bnd=list(x=-bnd$x,y=-bnd$y),-xx,-yy)
   gendata$x<-xx
   gendata$y<-yy
   
   # need to do this!!!
   gendata$inside<-onoff&onoff2
   
   ###################
   # create the grid
   horiz.set<-c()
   hind<-c()
   for(i in 1:19){
      horiz.set<-c(horiz.set,(1:res+(res*i*5)))
   }
   
   vert.set<-c()
   vind<-c()
   for(i in 1:res){
      vert.set<-c(vert.set,seq(1,res,5)+res*i)
   }
   
   
   hgrid.set<-list(x=gendata$x[horiz.set],
                   y=gendata$y[horiz.set])
   vgrid.set<-list(x=gendata$x[vert.set],
                   y=gendata$y[vert.set])
   
   na.ind<-!(is.na(vgrid.set$x)&is.na(vgrid.set$y))
   vgrid.set<-list(x=vgrid.set$x[na.ind],
                   y=vgrid.set$y[na.ind])
   na.ind<-!(is.na(hgrid.set$x)&is.na(hgrid.set$y))
   hgrid.set<-list(x=hgrid.set$x[na.ind],
                   y=hgrid.set$y[na.ind])
   onoff<-inSide(bnd=bnd,x=hgrid.set$x,y=hgrid.set$y)
   hgrid.set<-list(x=hgrid.set$x[onoff],
                   y=hgrid.set$y[onoff])

   onoff<-inSide(bnd=bnd,x=vgrid.set$x,y=vgrid.set$y)
   vgrid.set<-list(x=vgrid.set$x[onoff],
                   y=vgrid.set$y[onoff])

   ### get rid of duplicates
   mvec<-rep(TRUE,length(vgrid.set$x))

   for(j in 1:length(vgrid.set$x)){
      for(i in 1:length(hgrid.set$x)){
         if(abs(vgrid.set$x[j]-hgrid.set$x[i])<1e-6 &
            abs(vgrid.set$y[j]-hgrid.set$y[i])<1e-6){
            mvec[j]<-FALSE
         }
      }
   }

   return(list(x=c(vgrid.set$x[mvec],hgrid.set$x),
               y=c(vgrid.set$y[mvec],hgrid.set$y)))
} 
