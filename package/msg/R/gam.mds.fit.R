# fit a gam to general distance data
gam.mds.fit<-function(response,D,mds.dim=NULL,k=100,mds.dim.bnds=NULL,family=gaussian(),method="GCV.Cp",start.grid=NULL,samp.points=NULL,dist.metric="euclidean",select=FALSE){
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
   #  family         family arg to gam()
   #  start.grid     initial grid to use, can be NULL
   #  samp.points    sample points, needed if the above is specified    
   #  method         method= for gam() -- GCV.Cp and ML
   #  dist.metric    distance metric, anything allowable by dist() + 
   #                  "mahalanobis" + "hamming" + "spearman"
   #  select         do variable selection

   # Return - list
   #  $gam        gamObject of fitted model
   #  $mds.obj    object returned from cmdscale()
   #  $samp.mds   mds sample data.frame

   # big set of letters for column names
   bigletters<-as.vector(sapply(letters,paste,letters,sep=""))
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
   
      scores<-c() # store GCV/ML scores
      model.list<-list()
      i<-1 # counter
      
      for(test.dim in mds.bnds){
      
         # fit the model
         mds.dim<-test.dim

         gam.obj<-gam.fitter(response,D,mds.dim,k,family,samp.points,start.grid,method,dist.metric,select)

         # did it converge fully? split on outer/magic
         if(gam.obj$gam$optimizer[1]=="magic"){
            converged<-gam.obj$gam$mgcv.conv$fully.converged
         }else{
            converged<-gam.obj$gam$outer.info$conv=="full convergence"
         }

         # if the model is just linear, then discard it
         if(abs(as.double(length(attr(gam.obj$gam$terms,"variables")))-sum(gam.obj$gam$edf)) < sqrt(.Machine$double.eps)){
            converged<-FALSE
         }

         # don't include if we didn't get full convergence
         if(converged){
            model.list[[i]]<-gam.obj
      
#####
            if(method == "ML" | method == "P-ML"){
               ml.score<- 2*(model.list[[i]]$gam$gcv.ubre + (test.dim+1))
               scores<-c(scores,ml.score)
            }else{
            # extract the GCV
            scores<-c(scores,model.list[[i]]$gam$gcv.ubre)
            }
#####
         }else{
            model.list[[i]]<-NULL
            scores<-c(scores,NA)
         }

         i<-i+1
      }

      # if none of the models fit
      if(all(is.na(scores))){
         cat("No models fit!\n")
         return(NULL)
      }
      
      # now that's done, what was the smallest GCV?
      and.the.winner.is<-which.min(scores)
      
      fitted<-model.list[[and.the.winner.is]]
      mds.dim<-mds.bnds[and.the.winner.is]
      
      ret$scores<-data.frame(dim=mds.bnds,score=scores)


   }else if(floor(mds.dim)==mds.dim){
      fitted<-gam.fitter(response,D,mds.dim,k,family,samp.points,start.grid,method,dist.metric,select)
   }else{
      mds.dim<-choose.mds.dim(D,mds.dim)
      fitted<-gam.fitter(response,D,mds.dim,k,family,samp.points,start.grid,method,dist.metric,select)
   }

   ret$gam<-fitted$gam
   ret$mds.obj<-fitted$mds.obj
   ret$samp.mds<-fitted$samp.mds
   ret$mds.dim<-mds.dim

   return(ret)

}

### actually fit some GAMs
gam.fitter<-function(response,D,mds.dim,k,family,samp.points=NULL,grid.points=NULL,method,dist.metric="euclidean",select=FALSE){
   # big set of letters for column names
   bigletters<-as.vector(sapply(letters,paste,letters,sep=""))
   bl.len<-length(bigletters)

   ### Do the MDS projection 
   if(is.null(samp.points) & is.null(grid.points)){

      mds.obj<-cmdscale(D,mds.dim,eig=TRUE,k=mds.dim,x.ret=TRUE)
      samp.mds<-mds.obj$points
   
   }else if(!is.null(samp.points) & !is.null(grid.points)){

      if(dist.metric=="mahalanobis"){
         D.grid<-apply(grid.points,1,mahalanobis,x=grid.points,cov=cov(grid.points))
      }else if(dist.metric=="hamming"){
         D.grid<-hamming.distance(grid.points)
      }else if(dist.metric=="spearman"){
         D.grid<-spearman.dist(as.matrix(grid.points))
      }else{
         D.grid<-as.matrix(dist(grid.points,method=dist.metric))
      }

      mds.obj<-cmdscale(D.grid,mds.dim,eig=TRUE,k=mds.dim,x.ret=TRUE)
      samp.mds<-insert.mds.generic(mds.obj,samp.points,grid.points,dist.metric=dist.metric)

   }else{
      stop("Neither sample points or distance matrix supplied to gam.fitter\n")
   }

   samp.mds<-as.data.frame(cbind(response,samp.mds))
   colnames(samp.mds)<-c("response",bigletters[(bl.len-(dim(samp.mds)[2]-2)):bl.len])
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
   b<-gam(gam.formula,data=samp.mds,family=family,method=method,select=select)

   ret<-list(gam=b,mds.obj=mds.obj,samp.mds=samp.mds)

   return(ret)
}
