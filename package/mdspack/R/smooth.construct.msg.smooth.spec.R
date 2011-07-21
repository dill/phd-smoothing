smooth.construct.msg.smooth.spec<-function(object,data,knots){

   # this is the msg smooth.spec file
   # this does most of the work

   # for now TODO:
   #  just get spatial smoothing working
   #     do the clever stuff with previous objects as in gam.mds.R
   # THEN:
   #  if no bnd supplied do the general thing
   #  also do the s(.) thing
   #  select grid resolution sensibly

   # for the WAD case -- for non-WAD check that bnd exists
   if(length(names(data))!=2){
      stop("msg can only be used with 2D smooths!\n")
   }

   if(any(names(data)!=c("x","y"))){
      stop("Names of data are not \"x\" and \"y\"\n")
   }

   if(is.null(object$xt$mds.dim)){
      stop("no MDS projection dimension supplied!\n")
   }

   # extract the boundary
   bnd<-object$xt$bnd
   # extract the MDS dimension
   mds.dim<-object$xt$mds.dim

   grid.res<-object$xt$mds.grid.res
   if(is.null(grid.res)){
      # pick a grid size...

      # just pick something for now
      grid.res<-c(40,40)
   }

   # if there was an old object in the extra stuff, use it
   #old.obj<-object$xt$old.obj
   old.obj<-NULL

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
      grid.obj<-create_refgrid(bnd,grid.res)
      D.grid<-create_distance_matrix(grid.obj$x,grid.obj$y,bnd)
      grid.obj<-list(D=D.grid,grid=list(x=grid.obj$x,y=grid.obj$y))

      D.grid<-grid.obj$D
      my.grid<-grid.obj$grid
      # store!
      new.obj$D<-D.grid
      new.obj$grid<-my.grid

      object$msg<-new.obj

      D.samp<-NULL
      D.pred<-NULL
   }

   # store this stuff!!


   # now we have the grid object, insert the data into that
   # and store it as the data
   grid.mds<-cmdscale(D.grid,eig=TRUE,k=mds.dim,x.ret=TRUE)
   object$msg$grid.mds<-grid.mds
   mds.data<-as.data.frame(insert.mds(data,my.grid,grid.mds,bnd))
   object$msg$mds.data<-mds.data

   # make some variable names up
   mds.names<-paste("mds-",1:dim(mds.data)[2],sep="")
   # remove any already in the data
   names(mds.data)<-mds.names

   # make sure there are the right stuff is in the object before passing
   # to Duchon, but save beforehand!
   save.dim<-object$dim
   save.term<-object$term
   save.data<-data

   object$term<-mds.names
   object$dim<-mds.dim
   data<-mds.data

   object$msg$term<-mds.names
   object$msg$dim<-mds.dim
   object$msg$data<-mds.data

   # if knots were supplied, they're going to be ignored, warn about that!
   if(!is.null(knots)){
      warning("Knots were supplied but will be ignored!\n")
   }

   # make the duchon splines object as usual
   object<-smooth.construct.ds.smooth.spec(object,data,knots)

   if(object$xt$extra.penalty){
      object<-extra.penalty(object)
   }

   # recover the stuff we want in the object
   object$term<-save.term
   object$dim<-save.dim
   data<-save.data

   class(object)<-"msg.smooth"
   object
}

Predict.matrix.msg.smooth<-function(object,data){

   save.dim<-object$dim
   save.term<-object$term
   save.data<-data

   object$term<-object$msg$term
   object$dim<-object$msg$dim
   #data<-object$msg$data

   #### MAGIC HAPPENS HERE!!!!
   grid.mds<-object$msg$grid.mds
   my.grid<-object$msg$grid
   mds.data<-as.data.frame(insert.mds(data,my.grid,grid.mds,bnd))

   # make some variable names up
   mds.names<-paste("mds-",1:dim(mds.data)[2],sep="")
   # remove any already in the data
   names(mds.data)<-mds.names

   Predict.matrix.duchon.spline(object,mds.data)
}
