# function to do the squashing
squash<-function(x,lims,sq){
   # squash the points in x between lims[i] and lims[i+1] by a factor of sq

   x.ret<-c() # return vector

   x.tmp<-x[(x>=lims[1]) & (x<=lims[2])]
   x.tmp<-(x.tmp-lims[1])/sq[1] +lims[1]

   cc<-lims[2]/sq[1]

   x.ret<-c(x.ret,x.tmp)

   if(length(sq)>=2){
      for(i in 2:(length(sq))){
         x.tmp<-x[(x>lims[i]) & (x<=lims[i+1])]
         x.tmp<-(x.tmp-lims[i])/sq[i] +cc
         cc<-cc+(lims[i+1]-lims[i])/sq[i]
         x.ret<-c(x.ret,x.tmp)
      }
   }
   return(x.ret)
}

