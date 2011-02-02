gam.mds<-function(data,predp=NULL,bnd,mds.dim=2,grid.res=c(50,50),
#                 gam.options=list(bs="tp",k=100),
                  bs="tp",k=100,m=NULL,
                 old.obj=NULL){
   # general wrapper function for mds gam stuff


   # data            the data we actually want to do the smoothing over
   # predp           prediction points (if we want to do prediction)
   # bnd             the boundry in which the data lie
   # k               dimension of MDS projection
   # grid.res        resolution of the MDS grid
###   # gam.options     options to put in the s() term of the gam
   # old.obj         previous gam.mds object


   # TODO
   #  old.obj code
   #  non-geographical predictors
   #  predictors



   # first check that oldobj==NULL, otherwise use the data
   # from that rather than re-generating it!
   if(!is.null(old.obj)){
      # object to store all the results for later
      new.obj<-old.obj

      # if there is no sample then just do prediction

      D.grid<-old.obj$D
      my.grid<-old.obj$grid

      m<-old.obj$m
      bs<-old.obj$bs
      k<-old.obj$k

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

      new.obj$m<-m
      new.obj$bs<-bs
      new.obj$k<-k

   }

   grid.mds<-cmdscale(D.grid,eig=TRUE,k=mds.dim,x.ret=TRUE)
   
   new.obj$mds.dim<-mds.dim


   # map the samples

   ## according to the formula, what are the predictor names
   #predictors<-attr(terms(gam.formula),"term.labels")
   #response<-gam.formula[[2]]
   ## grab them
   #samp.data<-as.data.frame(data)[predictors]
   #response.var<-as.data.frame(data)[response]
   response.var<-data$z
   
   samp.data<-data

   #if(dim(samp.data)[2]!=2){
   #   die("data > 2 dimensions!\n")
   #}

   ## name them x and y
   #old.names<-names(samp.data)
   #names(samp.data)<-c("x","y")

   # insert the sample
   samp.mds<-insert.mds(samp.data,my.grid,grid.mds,bnd)

   #samp.mds<-as.data.frame(samp.mds)
   samp.mds<-as.data.frame(cbind(samp.mds,response.var))

   #names(samp.mds)<-c(old.names,response)
   names(samp.mds)<-letters[(26-(dim(samp.mds)[2]-1)):26] 

   # store!
   new.obj$samp.mds<-samp.mds

   ### fit the model

   # find the prediction terms
   pred.terms<-names(samp.mds)
   pred.terms<-pred.terms[-length(pred.terms)]
   pred.terms<-paste(pred.terms,sep=",")

   # create the gam formula
   if(is.null(m)){
      gam.options<-paste("bs='",bs,"', k=",k,sep="")
   }else if(!is.null(m) & length(m)==1){
      gam.options<-paste("bs='",bs,"', k=",k,", m=",m,sep="")
   }else{
      gam.options<-paste("bs='",bs,"', k=",k,", m=c(",m[1],",",m[2],")",sep="")
   }
   gam.formula<-paste("z~s(",paste(pred.terms,collapse=","),",",gam.options,")")
   gam.formula<-as.formula(gam.formula)

   # run the model
   b.mapped<-gam(gam.formula,data=samp.mds)
   # store
   new.obj$gam<-b.mapped

   # do the preictions
   if(!is.null(predp)){
      pred.mds<-insert.mds(predp,my.grid,grid.mds,bnd,faster=1)

      pred.mds<-as.data.frame(pred.mds)

      names(pred.mds)<-letters[(25-(dim(pred.mds)[2]-1)):25] 

      fv.mapped<-predict(b.mapped,newdata=pred.mds)

      #new.obj$pred<-cbind(predp,fv.mapped)
      new.obj$pred<-fv.mapped
   }

   # put everything back into the object

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
