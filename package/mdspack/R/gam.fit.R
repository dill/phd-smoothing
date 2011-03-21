# fit a gam to general distance data
gam.mds.fit<-function(response,D,mds.dim=NULL,k=100,mds.dim.bnds=NULL,fam=gaussian(),start.grid=NULL,samp.points=NULL){
   # Args
   #  response       vector of responses
   #  D              sample distance matrix (maybe generated using dist())
   #  mds.dim        dimension of MDS projection, NULL = select via GCV
   #                                        integer = dimension
   #                                        real = proportion of variation
   #  k              GAM basis dimension
   #  mds.dim.bnds   bounds for MDS dimension search --  lower always integer
   #                                                     upper int == bound
   #                                                     upper real == prop variation
   #  fam            family arg to gam()
   #  start.grid     initial grid to use, can be NULL
   #  samp.points    sample points, needed if the above is specified    

   # Return - list
   #  $gam        gamObject of fitted model
   #  $mds.obj    object returned from cmdscale()
   #  $samp.mds   mds sample data.frame

   # big set of letters for column names
   bigletters<-c(letters,paste("a",letters,sep=""),paste("b",letters,sep=""))
   bl.len<-length(bigletters)

   ret<-list()


   if(is.null(mds.dim)){
      # find the optimal MDS dimension
      if(is.null(mds.dim.bnds)){
         mds.bnds<-seq(2,choose.mds.dim(D,0.8),by=1)
      }else{
         if(mds.dim.bnds[2]==floor(mds.dim.bnds[2])){
            mds.bnds<-seq(mds.dim.bnds[1],mds.dim.bnds[2],by=1)
         }else{
            mds.bnds<-seq(mds.dim.bnds[1],choose.mds.dim(D,mds.dim.bnds[2]),by=1)
         }
      }
   
      gcvs<-c() # store GCV scores
      model.list<-list()
      i<-1 # counter
      
      for(test.dim in mds.bnds){
      
         # fit the model
         mds.dim<-test.dim
         model.list[[i]]<-gam.fitter(response,D,mds.dim,k,fam,samp.points,start.grid)
      
         # extract the GCV
         gcvs<-c(gcvs,model.list[[i]]$gam$gcv.ubre)
         i<-i+1
      #   if(i>2){
      #      if(gcvs[i-1]>gcvs[i-2]){
      #         break
      #      }
      #   }
      }
      
      # now that's done, what was the smallest GCV?
      and.the.winner.is<-which.min(gcvs)
      
      fitted<-model.list[[and.the.winner.is]]
      mds.dim<-mds.bnds[and.the.winner.is]
      
      ret$gcvs<-data.frame(gcv=gcvs,dim=mds.bnds)


   }else if(floor(mds.dim)==mds.dim){
      fitted<-gam.fitter(response,D,mds.dim,k,fam,samp.points,start.grid)
   }else{
      mds.dim<-choose.mds.dim(D,mds.dim)
      fitted<-gam.fitter(response,D,mds.dim,k,fam,samp.points,start.grid)
   }

   ret$gam<-fitted$gam
   ret$mds.obj<-fitted$mds.obj
   ret$samp.mds<-fitted$samp.mds
   ret$mds.dim<-mds.dim

   return(ret)

}

### actually fit some GAMs
gam.fitter<-function(response,D,mds.dim,k,fam,samp.points=NULL,grid.points=NULL){
   # big set of letters for column names
   bigletters<-c(letters,paste("a",letters,sep=""),paste("b",letters,sep=""))
   bl.len<-length(bigletters)


   ### Do the MDS projection
   if(is.null(samp.points) & is.null(grid.points)){

      mds.obj<-cmdscale(D,mds.dim,eig=TRUE,k=mds.dim,x.ret=TRUE)
      samp.mds<-mds.obj$points
   
   }else if(!is.null(samp.points) & !is.null(grid.points)){

      D.grid<-dist(grid.points,method="euclidean")

      mds.obj<-cmdscale(D.grid,mds.dim,eig=TRUE,k=mds.dim,x.ret=TRUE)
      samp.mds<-insert.mds.generic(mds.obj,samp.points,grid.points,dist.metric="euclidean")

   }else{
      stop("Neither sample points or distance matrix supplied to gan.fitter\n")
   }

   samp.mds<-cbind(response,samp.mds)
   attr(samp.mds,"dimnames")[[2]]<-c("response",
                                     bigletters[(bl.len-(dim(samp.mds)[2]-2)):bl.len])
   #attr(samp.mds,"dimnames")[[1]]<-mpid[samp.ind]
   samp.mds<-as.data.frame(samp.mds)
   
   # model setup
   m<-c(2,mds.dim/2-1)
   gam.options<-paste("bs='ds',k=",k,", m=c(",m[1],",",m[2],")",sep="")
   
   # find the prediction terms
   pred.terms<-bigletters[(bl.len-(dim(samp.mds)[2]-2)):bl.len]
   pred.terms<-paste(pred.terms,collapse=",")
   
   # create the gam formula
   gam.formula<-paste("response","~s(",paste(pred.terms,collapse=","),",",gam.options,")")
   gam.formula<-as.formula(gam.formula)
   
   # run the model
   b<-gam(gam.formula,data=samp.mds,family=fam)

   ret<-list(gam=b,mds.obj=mds.obj,samp.mds=samp.mds)

   return(ret)
}
