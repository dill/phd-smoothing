gam.mds<-function(samp.data,predp=NULL,bnd,mds.dim=NULL,grid.res=c(50,50),
                  bs="ds",k=100,m=NULL,family=gaussian(),old.obj=NULL,
                  gam.method="GCV.Cp"){
   ### general wrapper function for mds gam stuff
   # Args:
   #  samp.data    the data we actually want to do the smoothing over
   #  predp        prediction points (if we want to do prediction)
   #  bnd          the boundry in which the data lie
   #  mds.dim      dimension of MDS projection
   #  grid.res     resolution of the MDS grid
   #  bs           gam basis
   #  k            gam basis size
   #  m            m parameter for gam (not really used)
   #  old.obj      previous gam.mds object
   #  gam.method   what to use as method="" in the gam() call

   # TODO
   #  extra, non-geographical, predictors

   # first check that oldobj==NULL, otherwise use the data
   # from that rather than re-generating it!
   if(!is.null(old.obj)){
      # object to store all the results for later
      new.obj<-old.obj

      # pull out the grid D matrix
      D.grid<-old.obj$D
      my.grid<-old.obj$grid

      # also the pred and sample D matrices if
      # they are there
      if(!is.null(old.obj$D.samp)){
         D.samp<-old.obj$D.samp
      }else{
         D.samp<-NULL
      }
      if(!is.null(old.obj$D.pred)){
         D.pred<-old.obj$D.pred
      }else{
         D.pred<-NULL
      }

      if(!is.null(old.obj$m)){
         m<-old.obj$m
      }
      if(!is.null(old.obj$bs)){
         bs<-old.obj$bs
      }
      if(!is.null(old.obj$k)){
         k<-old.obj$k
      }

      if(!is.null(old.obj$mds.dim)){
         mds.dim<-old.obj$mds.dim
      }

   }else{
      # object to store all the results for later
      new.obj<-list()

      # create the grid
      #grid.obj<-calc.grid(bnd,grid.res)
      grid.obj<-create_refgrid(bnd,grid.res)
      D.grid<-create_distance_matrix(grid.obj$x,grid.obj$y,bnd)
      grid.obj<-list(D=D.grid,grid=list(x=grid.obj$x,y=grid.obj$y))

      D.grid<-grid.obj$D
      my.grid<-grid.obj$grid
      # store!
      new.obj$D<-D.grid
      new.obj$grid<-my.grid

      D.samp<-NULL
      D.pred<-NULL
   }


   # when mds.dim is not specified, do a grid search for the 
   # dimension
   if(is.null(mds.dim)){
      # find the bounds for the dimensions of the MDS projection
      # min is 2; max is whatever gives 95% variation
      mds.bnds<-seq(2,choose.mds.dim(D.grid,0.95),by=1)
      
      gcvs<-c() # store GCV scores
      model.list<-list()
      i<-1 # counter

      for(test.dim in mds.bnds){
      #while(gcvs[i-1]<=gcvs[i-2]){
      
         # fit the model
         model.list[[i]]<-run.gam(samp.data,D.grid,test.dim,my.grid,bnd,
                                  D.samp,family,m,k,bs,gam.method)

         # if we didn't calculate D.samp before, calculate it the first time
         # then store it for the future runs
         if(is.null(D.samp)){
            D.samp<-model.list[[i]]$D.samp
         }
         # extract the GCV
         gcvs<-c(gcvs,model.list[[i]]$gam$gcv.ubre)
         i<-i+1
         #if(i>2){
         #   if(gcvs[i-1]>gcvs[i-2]){ 
         #      break
         #   }
         #}
      }

      # now that's done, what was the smallest GCV?
      and.the.winner.is<-which.min(gcvs)

      fit.ret<-model.list[[and.the.winner.is]]

      fit.ret$gcv.dim<-cbind(mds.bnds,gcvs)

   # otherwise, do what the user wants
   }else{

      # if there was a non-integer supplied, then we can
      # assume that we want what proportion of the variance 
      # explained...
      if(mds.dim!=floor(mds.dim)){
         lev<-mds.dim
         mds.dim<-choose.mds.dim(D.grid,mds.dim)
         new.obj$prop.explained<-lev
         cat("mds.dim=",mds.dim,"explains",lev,"of the variance\n")
      }

      # otherwise the user supplied a number of dimensions to use
      fit.ret<-run.gam(samp.data,D.grid,mds.dim,my.grid,bnd,D.samp,family,m,k,bs,gam.method)
   }

   # store some objects
   new.obj$samp.mds<-fit.ret$samp.mds
   new.obj$gam<-fit.ret$gam
   new.obj$D.samp<-fit.ret$D.samp
   grid.mds<-fit.ret$grid.mds

   ###########################################################################
   # do the preictions
   if(!is.null(predp)){
      # insert the sample - if there was a D cached, use that
      if(is.null(D.pred)){
         pred.mds<-insert.mds(predp,my.grid,grid.mds,bnd,faster=1)
         new.obj$D.pred<-attr(pred.mds,"D")
      }else{
         pred.mds<-insert.mds(predp,my.grid,grid.mds,bnd,faster=1,oldD=D.pred)
      }

      pred.mds<-as.data.frame(pred.mds)

      names(pred.mds)<-letters[(25-(dim(pred.mds)[2]-1)):25] 

      fv.mapped<-predict(new.obj$gam,newdata=pred.mds,type="response")

      new.obj$pred<-fv.mapped
   }

   # put everything back into the object
   new.obj$m<-fit.ret$m
   new.obj$bs<-fit.ret$bs
   new.obj$k<-fit.ret$k
   new.obj$mds.dim<-fit.ret$mds.dim

   return(new.obj)
}



calc.grid<-function(bnd,grid.res){
   # create a grid
   xm <- seq(-1,3.5,length=grid.res[1])
   yn<-seq(-1,1,length=grid.res[2])
   xx <- rep(xm,grid.res[2])
   yy<-rep(yn,rep(grid.res[1],grid.res[2]))
   onoff<-inSide(bnd,xx,yy)
   xx<-xx[onoff];yy<-yy[onoff]

   # map the grid
   my.grid<-list(x=xx,y=yy)
   D.grid<-create_distance_matrix(xx,yy,bnd)
   return(list(D=D.grid,grid=my.grid))
}



# routine to actually fit the model for a set dimension
run.gam<-function(samp.data,D.grid,mds.dim,my.grid,bnd,D.samp,family,m,k,bs,gam.method){

   # set the gam options
   if(is.null(m)){
      # if we have Duchon basis use s=d/2-1
      if(bs=="ds"){
         m<-c(2,mds.dim/2-1)
         gam.options<-paste("bs='",bs,"', k=",k,", m=c(",m[1],",",m[2],")",sep="")
      }else{
         gam.options<-paste("bs='",bs,"', k=",k,sep="")
      }
   }else if(!is.null(m) & length(m)==1){
      gam.options<-paste("bs='",bs,"', k=",k,", m=",m,sep="")
   }else{
      gam.options<-paste("bs='",bs,"', k=",k,", m=c(",m[1],",",m[2],")",sep="")
   }

   # project the grid at the set dimension
   grid.mds<-cmdscale(D.grid,eig=TRUE,k=mds.dim,x.ret=TRUE)
   
   # insert the sample - if there was a D cached, use that
   if(is.null(D.samp)){
      samp.mds<-insert.mds(samp.data,my.grid,grid.mds,bnd)
      D.samp<-attr(samp.mds,"D")
   }else{
      samp.mds<-insert.mds(samp.data,my.grid,grid.mds,bnd,oldD=D.samp)
   }

   # make the sample data frame
   samp.mds<-as.data.frame(cbind(samp.mds,samp.data$z))
   # name the columns
   names(samp.mds)<-letters[(26-(dim(samp.mds)[2]-1)):26] 

   # find the prediction terms
   pred.terms<-names(samp.mds)
   pred.terms<-pred.terms[-length(pred.terms)]
   pred.terms<-paste(pred.terms,sep=",")

   # create the gam formula
   gam.formula<-paste("z~s(",paste(pred.terms,collapse=","),",",gam.options,")")
   gam.formula<-as.formula(gam.formula)

   # run the model
   b<-gam(gam.formula,data=samp.mds,family=family,method=gam.method)

   return(list(gam=b,samp.mds=samp.mds,grid.mds=grid.mds,m=m,k=k,
               bs=bs,mds.dim=mds.dim,D.samp=D.samp))
}

